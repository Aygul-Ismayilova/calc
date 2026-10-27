// Aygul Ismayilova 251ADB153
// https://github.com/Aygul-Ismayilova/calc
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: C standard library documentation, 
//  AI (Gemini) for algorithmic structure of recursive descent parser and memory buffer file reading.
// link: https://share.gemini.google/15cVODMdsNsM
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>

#define NAME       "aygul"
#define LASTNAME   "ismayilova"
#define STUDENT_ID "251ADB153"

/* small helpers  */

static void *xmalloc(size_t n)
{
    void *p = malloc(n ? n : 1);
    if (!p) {
        fprintf(stderr, "calc: out of memory\n");
        exit(2);
    }
    return p;
}

static int is_digit(char c) { return c >= '0' && c <= '9'; }

/* Growable output buffer. Results are collected here instead of being
 * written straight to the file, because one error anywhere in the file
 * means the file must contain only ERROR:<pos> and no partial results.
 * The capacity doubles when full, so appending n bytes in total costs
 * O(n): every byte is copied a constant number of times on average. */
typedef struct {
    char *data;
    size_t len, cap;
} Buf;

static void buf_append(Buf *b, const char *s, size_t n)
{
    if (b->len + n > b->cap) {
        size_t cap = b->cap ? b->cap : 4096;
        while (cap < b->len + n)
            cap *= 2;
        char *d = realloc(b->data, cap);
        if (!d) {
            fprintf(stderr, "calc: out of memory\n");
            exit(2);
        }
        b->data = d;
        b->cap = cap;
    }
    memcpy(b->data + b->len, s, n);
    b->len += n;
}

/* tokenizer  */

typedef enum {
    TOK_INT, TOK_FLOAT,
    TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH, TOK_POW,
    TOK_LPAREN, TOK_RPAREN,
    TOK_END,     /* end of the line: its '\n', or the end of the file */
    TOK_INVALID  /* a character that cannot start any token */
} TokType;

typedef struct {
    TokType type;
    size_t pos;      /* index in the whole file of the token's first char */
    long long ival;  /* value of TOK_INT */
    double fval;     /* value of TOK_FLOAT */
} Token;

/* The whole file is in one buffer and the tokenizer walks it with an index,
 * so a token's position is simply its index + 1. Nothing is counted
 * separately, so comment lines, blank lines and every '\n' are included in
 * the positions automatically. */
typedef struct {
    char *text;       /* whole file, text[len] == '\0' */
    size_t cur;       /* index of the first character not yet tokenized */
    size_t line_end;  /* index of this line's '\n', or the file length */
    Token tok;        /* the token the parser is looking at right now */
    size_t err_pos;   /* index of the error, set when a function returns 0 */
} Parser;

static void next_token(Parser *p)
{
    char *s = p->text;
    size_t i = p->cur;
    Token t = {TOK_END, 0, 0, 0.0};

    while (i < p->line_end && (s[i] == ' ' || s[i] == '\t'))
        i++;
    t.pos = i;
    /* When the line is used up, the END token sits on the '\n' itself (or
     * one past the last character if the file has no final '\n'). That is
     * exactly where "missing operand" and "missing )" must be reported. */
    if (i >= p->line_end) {
        p->cur = i;
        p->tok = t;
        return;
    }

    if (is_digit(s[i])) {
        size_t j = i;
        while (j < p->line_end && is_digit(s[j]))
            j++;
        /* A float needs digits on both sides of the '.'. For "5." or "5.x"
         * the number ends before the '.', and the '.' becomes an invalid
         * token, so the error lands on the '.' as the rules require. */
        if (j + 1 < p->line_end && s[j] == '.' && is_digit(s[j + 1])) {
            j++;
            while (j < p->line_end && is_digit(s[j]))
                j++;
            /* strtod would also accept an exponent such as "1.5e3", which
             * our grammar forbids, so the string is cut right after the
             * number for the duration of the call. The 'e' then becomes an
             * invalid token and the error is reported there. */
            char saved = s[j];
            s[j] = '\0';
            t.fval = strtod(s + i, NULL);
            s[j] = saved;
            t.type = TOK_FLOAT;
        } else {
            t.ival = strtoll(s + i, NULL, 10);  /* stops at the first non-digit */
            t.type = TOK_INT;
        }
        p->cur = j;
        p->tok = t;
        return;
    }

    p->cur = i + 1;
    switch (s[i]) {
    case '+': t.type = TOK_PLUS;   break;
    case '-': t.type = TOK_MINUS;  break;
    case '/': t.type = TOK_SLASH;  break;
    case '(': t.type = TOK_LPAREN; break;
    case ')': t.type = TOK_RPAREN; break;
    case '*':
        /* "**" is read as one token, so "2 ** 3" is a power and "2 * * 3"
         * is two operators in a row. */
        if (i + 1 < p->line_end && s[i + 1] == '*') {
            t.type = TOK_POW;
            p->cur = i + 2;
        } else {
            t.type = TOK_STAR;
        }
        break;
    default:
        /* '#' outside a comment line, '&', letters, '.', ... The parser can
         * never accept this token, so it stops here and the error position
         * is this character. */
        t.type = TOK_INVALID;
        break;
    }
    p->tok = t;
}

/*  arithmetic  */

typedef struct {
    int is_float;
    long long i;
    double f;
} Value;

static int fail(Parser *p, size_t pos)
{
    p->err_pos = pos;
    return 0;
}

static double as_double(Value v) { return v.is_float ? v.f : (double)v.i; }

/* Integer power by repeated squaring: O(log e) multiplications, so even
 * 1 ** 1000000000000 is instant. The base is squared only while bits of the
 * exponent remain, so no multiplication is done whose result is not needed
 * (an unneeded last squaring could overflow even when the answer fits). */
static long long int_pow(long long base, long long e)
{
    long long result = 1;
    while (e > 0) {
        if (e & 1)
            result *= base;
        e >>= 1;
        if (e > 0)
            base *= base;
    }
    return result;
}

/* a = a <op> b for + - * /.
 * Integer vs float: the operation stays in long long only when both
 * operands are integers; if either one is a float, both are converted to
 * double. So 7 / 2 is 3 (C truncation) but 7 / 2.0 is 3.5.
 * right_pos is where the right operand starts: a division by zero is
 * reported there, because that operand is what makes the division
 * impossible. op_pos is the operator: an infinite/NaN float result is
 * reported there, because the operation (not an operand) produced it. */
static int apply(Parser *p, Value *a, TokType op, Value b,
                 size_t op_pos, size_t right_pos)
{
    if (!a->is_float && !b.is_float) {
        switch (op) {
        case TOK_PLUS:  a->i += b.i; break;
        case TOK_MINUS: a->i -= b.i; break;
        case TOK_STAR:  a->i *= b.i; break;
        default:  /* TOK_SLASH */
            if (b.i == 0)
                return fail(p, right_pos);
            a->i /= b.i;  /* C division truncates toward zero, as required */
            break;
        }
        return 1;
    }

    double x = as_double(*a), y = as_double(b), r;
    switch (op) {
    case TOK_PLUS:  r = x + y; break;
    case TOK_MINUS: r = x - y; break;
    case TOK_STAR:  r = x * y; break;
    default:  /* TOK_SLASH */
        if (y == 0.0)
            return fail(p, right_pos);
        r = x / y;
        break;
    }
    if (!isfinite(r))
        return fail(p, op_pos);
    a->is_float = 1;
    a->f = r;
    return 1;
}

/* base = base ** e. Only int ** int can have a "negative exponent" error:
 * with a float on either side the power is done in double, where a negative
 * exponent is fine (0.5 ** (0 - 1) is 2). The error goes to exp_pos, the
 * start of the exponent, because the exponent is the bad value. */
static int apply_pow(Parser *p, Value *base, Value e,
                     size_t op_pos, size_t exp_pos)
{
    if (!base->is_float && !e.is_float) {
        if (e.i < 0)
            return fail(p, exp_pos);
        base->i = int_pow(base->i, e.i);  /* x ** 0 is 1, also 0 ** 0 */
        return 1;
    }
    double r = pow(as_double(*base), as_double(e));
    if (!isfinite(r))  /* e.g. 10.0 ** 400 (inf) or (0 - 8) ** 0.5 (NaN) */
        return fail(p, op_pos);
    base->is_float = 1;
    base->f = r;
    return 1;
}

/* parser 
 * Recursive descent, one function per precedence level:
 *   expr    := term  { ('+' | '-') term }
 *   term    := power { ('*' | '/') power }
 *   power   := primary [ '**' power ]
 *   primary := NUMBER | '(' expr ')'
 * Precedence comes from the call structure: expr can only combine whole
 * terms, and a term is finished (all its * and / applied) before expr sees
 * the next '+'. So in 3 + 2 * 5 the 2 * 5 is done first. In the same way
 * a power is finished before term sees a '*'.
 * + - * / are left-associative because their loops combine the result so
 * far with the next operand, left to right: 10 - 3 - 2 is (10 - 3) - 2.
 * ** is right-associative because power() calls itself for the exponent,
 * so 2 ** 3 ** 2 is parsed as 2 ** (3 ** 2).
 *
 * The parser evaluates as it goes. Every function returns 1 on success or
 * 0 after storing the error position. The rule "the error is at the token
 * the parser was looking at when it could not continue" is then simply
 * p->tok.pos at the moment we give up: a second operator, an invalid
 * character, a ')' with no '(' or the END of the line. */

static int parse_expr(Parser *p, Value *out);

static int parse_primary(Parser *p, Value *out)
{
    Token t = p->tok;
    switch (t.type) {
    case TOK_INT:
        out->is_float = 0;
        out->i = t.ival;
        next_token(p);
        return 1;
    case TOK_FLOAT:
        out->is_float = 1;
        out->f = t.fval;
        next_token(p);
        return 1;
    case TOK_LPAREN:
        next_token(p);
        if (!parse_expr(p, out))
            return 0;
        /* A missing ')' is noticed here, on whatever token came instead:
         * usually END, so the error is at the end of the line. */
        if (p->tok.type != TOK_RPAREN)
            return fail(p, p->tok.pos);
        next_token(p);
        return 1;
    default:
        /* An operand was needed but this token cannot start one. This one
         * place covers "two operators in a row" (we see the 2nd operator),
         * "-5" (no unary minus), "()" (we see the ')'), invalid characters
         * and a missing operand at the end of the line (we see END). */
        return fail(p, t.pos);
    }
}

static int parse_power(Parser *p, Value *out)
{
    if (!parse_primary(p, out))
        return 0;
    if (p->tok.type != TOK_POW)
        return 1;
    size_t op_pos = p->tok.pos;
    next_token(p);
    size_t exp_pos = p->tok.pos;  /* start of the whole exponent */
    Value e;
    if (!parse_power(p, &e))  /* recursion = right-associative */
        return 0;
    return apply_pow(p, out, e, op_pos, exp_pos);
}

static int parse_term(Parser *p, Value *out)
{
    if (!parse_power(p, out))
        return 0;
    while (p->tok.type == TOK_STAR || p->tok.type == TOK_SLASH) {
        TokType op = p->tok.type;
        size_t op_pos = p->tok.pos;
        next_token(p);
        size_t right_pos = p->tok.pos;
        Value r;
        if (!parse_power(p, &r))
            return 0;
        if (!apply(p, out, op, r, op_pos, right_pos))
            return 0;
    }
    return 1;
}

static int parse_expr(Parser *p, Value *out)
{
    if (!parse_term(p, out))
        return 0;
    while (p->tok.type == TOK_PLUS || p->tok.type == TOK_MINUS) {
        TokType op = p->tok.type;
        size_t op_pos = p->tok.pos;
        next_token(p);
        size_t right_pos = p->tok.pos;
        Value r;
        if (!parse_term(p, &r))
            return 0;
        if (!apply(p, out, op, r, op_pos, right_pos))
            return 0;
    }
    return 1;
}

/* Evaluates one line text[start .. end), where end is the line's '\n' or
 * the end of the file. */
static int eval_line(Parser *p, size_t start, size_t end, Value *out)
{
    p->cur = start;
    p->line_end = end;
    next_token(p);
    if (!parse_expr(p, out))
        return 0;
    /* A complete expression must use up the whole line. Anything left over
     * (an extra ')', a second number, an invalid character) is the token
     * the parser could not continue with. */
    if (p->tok.type != TOK_END)
        return fail(p, p->tok.pos);
    return 1;
}

/* Evaluates every expression line of text[0 .. len) into out. On the first
 * error, out is emptied and holds only ERROR:<pos>. */
static void eval_file(char *text, size_t len, Buf *out)
{
    Parser p;
    char line[64];
    size_t start = 0;

    p.text = text;
    while (start < len) {
        /* memchr finds the end of the line without copying it anywhere;
         * the parser then works on the line in place. */
        const char *nl = memchr(text + start, '\n', len - start);
        size_t end = nl ? (size_t)(nl - text) : len;
        size_t i = start;
        while (i < end && (text[i] == ' ' || text[i] == '\t'))
            i++;

        /* Blank lines and comment lines produce no output. */
        if (i < end && text[i] != '#') {
            Value v;
            if (!eval_line(&p, start, end, &v)) {
                out->len = 0;  /* drop results of earlier lines */
                int n = snprintf(line, sizeof line, "ERROR:%zu\n", p.err_pos + 1);
                buf_append(out, line, (size_t)n);
                return;
            }
            int n = v.is_float ? snprintf(line, sizeof line, "%.15g\n", v.f)
                               : snprintf(line, sizeof line, "%lld\n", v.i);
            buf_append(out, line, (size_t)n);
        }
        start = end + 1;
    }
}

/*  files and folders  */

/* Reads the whole file into memory with a single fread. Knowing the size
 * first means the data is read once and never copied again. One extra
 * byte holds a '\0' so strtod/strtoll always stop inside the buffer. */
static char *read_file(const char *path, size_t *len_out)
{
    FILE *f = fopen(path, "rb");
    if (!f)
        return NULL;
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        return NULL;
    }
    long size = ftell(f);
    if (size < 0 || fseek(f, 0, SEEK_SET) != 0) {
        fclose(f);
        return NULL;
    }
    char *text = xmalloc((size_t)size + 1);
    size_t got = fread(text, 1, (size_t)size, f);
    int bad = ferror(f);
    fclose(f);
    if (bad) {
        free(text);
        return NULL;
    }
    text[got] = '\0';
    *len_out = got;
    return text;
}

/* Copies the last part of a path ("labs/task1.txt" -> "task1.txt",
 * "labs/" -> "labs") and, if strip_txt is set, removes a final ".txt". */
static char *base_name(const char *path, int strip_txt)
{
    size_t end = strlen(path);
    while (end > 1 && path[end - 1] == '/')
        end--;
    size_t start = end;
    while (start > 0 && path[start - 1] != '/')
        start--;
    if (strip_txt && end - start > 4 && strncmp(path + end - 4, ".txt", 4) == 0)
        end -= 4;
    char *s = xmalloc(end - start + 1);
    memcpy(s, path + start, end - start);
    s[end - start] = '\0';
    return s;
}

/* Default output folder: <input_base>_<name>_<studentid> */
static char *default_outdir(const char *base)
{
    size_t n = strlen(base) + sizeof("_" NAME "_" STUDENT_ID);
    char *s = xmalloc(n);
    snprintf(s, n, "%s_%s_%s", base, NAME, STUDENT_ID);
    return s;
}

/* <outdir>/<base>_<name>_<lastname>_<studentid>.txt */
static char *output_path(const char *outdir, const char *base)
{
    size_t n = strlen(outdir) + strlen(base) +
               sizeof("/_" NAME "_" LASTNAME "_" STUDENT_ID ".txt");
    char *s = xmalloc(n);
    snprintf(s, n, "%s/%s_%s_%s_%s.txt", outdir, base, NAME, LASTNAME, STUDENT_ID);
    return s;
}

/* Makes sure outdir is a folder we can write into: an existing folder is
 * used as it is, a missing one is created. */
static int ensure_dir(const char *outdir)
{
    struct stat st;
    if (stat(outdir, &st) == 0) {
        if (S_ISDIR(st.st_mode))
            return 1;
        fprintf(stderr, "calc: '%s' exists and is not a folder\n", outdir);
        return 0;
    }
    if (mkdir(outdir, 0777) != 0) {
        fprintf(stderr, "calc: cannot create folder '%s': %s\n", outdir, strerror(errno));
        return 0;
    }
    return 1;
}

/* Evaluates one input file and writes its output file.
 * Returns 1 on success, 0 if the input could not be read or the output
 * could not be written (an ERROR:<pos> result is a success). */
static int process_file(const char *in_path, const char *outdir)
{
    size_t len = 0;
    char *text = read_file(in_path, &len);
    if (!text) {
        fprintf(stderr, "calc: cannot read '%s': %s\n", in_path, strerror(errno));
        return 0;
    }

    Buf out = {NULL, 0, 0};
    eval_file(text, len, &out);
    free(text);

    char *base = base_name(in_path, 1);
    char *path = output_path(outdir, base);
    free(base);

    int ok = 1;
    FILE *f = fopen(path, "w");
    if (!f) {
        fprintf(stderr, "calc: cannot write '%s': %s\n", path, strerror(errno));
        ok = 0;
    } else {
        if (out.len > 0 && fwrite(out.data, 1, out.len, f) != out.len)
            ok = 0;
        if (fclose(f) != 0)
            ok = 0;
        if (!ok)
            fprintf(stderr, "calc: error writing '%s'\n", path);
    }
    free(path);
    free(out.data);
    return ok;
}

static int is_regular_file(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

static int ends_with_txt(const char *name)
{
    size_t n = strlen(name);
    return n > 4 && strcmp(name + n - 4, ".txt") == 0;
}

static int cmp_names(const void *a, const void *b)
{
    return strcmp(*(char *const *)a, *(char *const *)b);
}

/* Evaluates every *.txt file directly inside dir.
 * The names are collected first and the folder is closed before any output
 * is written, so output files created inside dir (calc -d x -o x) are never
 * picked up as new inputs. Sorting only makes the order predictable. */
static int process_dir(const char *dir, const char *outdir)
{
    DIR *d = opendir(dir);
    if (!d) {
        fprintf(stderr, "calc: cannot open folder '%s': %s\n", dir, strerror(errno));
        return 0;
    }
    char **names = NULL;
    size_t count = 0, cap = 0;
    struct dirent *e;
    while ((e = readdir(d)) != NULL) {
        if (!ends_with_txt(e->d_name))
            continue;
        if (count == cap) {
            cap = cap ? cap * 2 : 16;
            char **grown = realloc(names, cap * sizeof *names);
            if (!grown) {
                fprintf(stderr, "calc: out of memory\n");
                exit(2);
            }
            names = grown;
        }
        size_t n = strlen(e->d_name) + 1;
        names[count] = xmalloc(n);
        memcpy(names[count], e->d_name, n);
        count++;
    }
    closedir(d);
    if (count > 1)
        qsort(names, count, sizeof *names, cmp_names);

    int ok = 1;
    for (size_t k = 0; k < count; k++) {
        size_t n = strlen(dir) + strlen(names[k]) + 2;
        char *path = xmalloc(n);
        snprintf(path, n, "%s/%s", dir, names[k]);
        /* Subfolders (even one called x.txt) and other non-regular files
         * are skipped, as the task says. */
        if (is_regular_file(path) && !process_file(path, outdir))
            ok = 0;
        free(path);
        free(names[k]);
    }
    free(names);
    return ok;
}

/*  command line */

static void usage(void)
{
    fprintf(stderr,
            "usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
}

int main(int argc, char *argv[])
{
    const char *dir = NULL, *outdir = NULL, *input = NULL;

    for (int k = 1; k < argc; k++) {
        const char *a = argv[k];
        int is_dir = strcmp(a, "-d") == 0 || strcmp(a, "--dir") == 0;
        int is_out = strcmp(a, "-o") == 0 || strcmp(a, "--output-dir") == 0;
        if (is_dir || is_out) {
            if (k + 1 >= argc) {
                fprintf(stderr, "calc: option '%s' needs a value\n", a);
                usage();
                return 1;
            }
            k++;
            if (is_dir)
                dir = argv[k];
            else
                outdir = argv[k];
        } else if (a[0] == '-') {
            fprintf(stderr, "calc: unknown option '%s'\n", a);
            usage();
            return 1;
        } else if (input) {
            fprintf(stderr, "calc: only one input file is allowed\n");
            usage();
            return 1;
        } else {
            input = a;
        }
    }

    if (!dir && !input) {
        usage();
        return 1;
    }
    if (dir && input) {
        fprintf(stderr, "calc: give either -d DIR or an input file, not both\n");
        usage();
        return 1;
    }

    /* Check the input before creating any output folder, so a typo in the
     * input name does not leave an empty folder behind. */
    struct stat st;
    const char *in = dir ? dir : input;
    if (stat(in, &st) != 0) {
        fprintf(stderr, "calc: cannot open '%s': %s\n", in, strerror(errno));
        return 1;
    }
    if (dir ? !S_ISDIR(st.st_mode) : !S_ISREG(st.st_mode)) {
        fprintf(stderr, "calc: '%s' is not a %s\n", in, dir ? "folder" : "file");
        return 1;
    }

    char *made_outdir = NULL;
    if (!outdir) {
        char *base = base_name(in, dir == NULL);  /* strip .txt only for a file */
        made_outdir = default_outdir(base);
        free(base);
        outdir = made_outdir;
    }

    int ok = ensure_dir(outdir) &&
             (dir ? process_dir(dir, outdir) : process_file(input, outdir));
    free(made_outdir);
    return ok ? 0 : 1;
}