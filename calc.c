// Aygul Ismayilova 251ADB153
// https://github.com/Aygul-Ismayilova/calc
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define NAME       "aygul"
#define LASTNAME   "ismayilova"
#define STUDENT_ID "251ADB153"

/* ---------- small helpers ---------- */

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

/* ---------- tokenizer ---------- */

typedef enum {
    TOK_INT,
    TOK_PLUS, TOK_MINUS,
    TOK_END,     /* end of the line: its '\n', or the end of the file */
    TOK_INVALID  /* a character that cannot start any token */
} TokType;

typedef struct {
    TokType type;
    size_t pos;      /* index in the whole file of the token's first char */
    long long ival;  /* value of TOK_INT */
} Token;

/* The whole file is in one buffer and the tokenizer walks it with an index,
 * so a token's position is simply its index + 1. Nothing is counted
 * separately, so blank lines and every '\n' are included in the positions
 * automatically. */
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
    Token t = {TOK_END, 0, 0};

    while (i < p->line_end && (s[i] == ' ' || s[i] == '\t'))
        i++;
    t.pos = i;
    /* When the line is used up, the END token sits on the '\n' itself (or
     * one past the last character if the file has no final '\n'). That is
     * exactly where a missing operand must be reported. */
    if (i >= p->line_end) {
        p->cur = i;
        p->tok = t;
        return;
    }

    if (is_digit(s[i])) {
        size_t j = i;
        while (j < p->line_end && is_digit(s[j]))
            j++;
        t.ival = strtoll(s + i, NULL, 10);  /* stops at the first non-digit */
        t.type = TOK_INT;
        p->cur = j;
        p->tok = t;
        return;
    }

    p->cur = i + 1;
    switch (s[i]) {
    case '+': t.type = TOK_PLUS;  break;
    case '-': t.type = TOK_MINUS; break;
    default:
        /* '&', letters, ... The parser can never accept this token, so it
         * stops here and the error position is this character. */
        t.type = TOK_INVALID;
        break;
    }
    p->tok = t;
}

/* ---------- parser ----------
 * Recursive descent. For now there is only one precedence level:
 *   expr    := primary { ('+' | '-') primary }
 *   primary := NUMBER
 * + and - are left-associative because the loop in parse_expr combines the
 * result so far with the next operand, left to right: 10 - 3 - 2 is
 * (10 - 3) - 2 = 5.
 *
 * The parser evaluates as it goes. Every function returns 1 on success or
 * 0 after storing the error position. The rule "the error is at the token
 * the parser was looking at when it could not continue" is then simply
 * p->tok.pos at the moment we give up. */

static int fail(Parser *p, size_t pos)
{
    p->err_pos = pos;
    return 0;
}

static int parse_primary(Parser *p, long long *out)
{
    if (p->tok.type == TOK_INT) {
        *out = p->tok.ival;
        next_token(p);
        return 1;
    }
    /* An operand was needed but this token cannot start one. This one
     * place covers "two operators in a row" (we see the 2nd operator),
     * "-5" (there is no unary minus), invalid characters, and a missing
     * operand at the end of the line (we see END). */
    return fail(p, p->tok.pos);
}

static int parse_expr(Parser *p, long long *out)
{
    if (!parse_primary(p, out))
        return 0;
    while (p->tok.type == TOK_PLUS || p->tok.type == TOK_MINUS) {
        TokType op = p->tok.type;
        next_token(p);
        long long r;
        if (!parse_primary(p, &r))
            return 0;
        if (op == TOK_PLUS)
            *out += r;
        else
            *out -= r;
    }
    return 1;
}

/* Evaluates one line text[start .. end), where end is the line's '\n' or
 * the end of the file. */
static int eval_line(Parser *p, size_t start, size_t end, long long *out)
{
    p->cur = start;
    p->line_end = end;
    next_token(p);
    if (!parse_expr(p, out))
        return 0;
    /* A complete expression must use up the whole line. Anything left over
     * (a second number, an invalid character) is the token the parser
     * could not continue with. */
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

        /* Blank lines produce no output. */
        if (i < end) {
            long long v;
            if (!eval_line(&p, start, end, &v)) {
                out->len = 0;  /* drop results of earlier lines */
                int n = snprintf(line, sizeof line, "ERROR:%zu\n", p.err_pos + 1);
                buf_append(out, line, (size_t)n);
                return;
            }
            int n = snprintf(line, sizeof line, "%lld\n", v);
            buf_append(out, line, (size_t)n);
        }
        start = end + 1;
    }
}

/* ---------- files and folders ---------- */

/* Reads the whole file into memory with a single fread. Knowing the size
 * first means the data is read once and never copied again. One extra
 * byte holds a '\0' so strtoll always stops inside the buffer. */
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

/* Copies the file name out of a path ("labs/task1.txt" -> "task1") and
 * removes a final ".txt". */
static char *base_name(const char *path)
{
    size_t end = strlen(path);
    size_t start = end;
    while (start > 0 && path[start - 1] != '/')
        start--;
    if (end - start > 4 && strncmp(path + end - 4, ".txt", 4) == 0)
        end -= 4;
    char *s = xmalloc(end - start + 1);
    memcpy(s, path + start, end - start);
    s[end - start] = '\0';
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

/* An existing output folder is used as it is; a missing one is created. */
static int ensure_dir(const char *outdir)
{
    struct stat st;
    if (stat(outdir, &st) == 0 && S_ISDIR(st.st_mode))
        return 1;
    if (mkdir(outdir, 0777) != 0) {
        fprintf(stderr, "calc: cannot create folder '%s'\n", outdir);
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
        fprintf(stderr, "calc: cannot read '%s'\n", in_path);
        return 0;
    }

    Buf out = {NULL, 0, 0};
    eval_file(text, len, &out);
    free(text);

    char *base = base_name(in_path);
    char *path = output_path(outdir, base);
    free(base);

    int ok = 1;
    FILE *f = fopen(path, "w");
    if (!f) {
        fprintf(stderr, "calc: cannot write '%s'\n", path);
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

/* ---------- command line ----------
 * For now only "calc -o OUTDIR input.txt" (grade 4). The full command line
 * from section 4 (-d, default output folder) comes later. */

int main(int argc, char *argv[])
{
    const char *outdir = NULL, *input = NULL;

    for (int k = 1; k < argc; k++) {
        if (strcmp(argv[k], "-o") == 0 || strcmp(argv[k], "--output-dir") == 0) {
            if (k + 1 >= argc) {
                fprintf(stderr, "calc: option '%s' needs a value\n", argv[k]);
                return 1;
            }
            outdir = argv[++k];
        } else {
            input = argv[k];
        }
    }
    if (!outdir || !input) {
        fprintf(stderr, "usage: calc -o OUTDIR input.txt\n");
        return 1;
    }

    if (!ensure_dir(outdir) || !process_file(input, outdir))
        return 1;
    return 0;
}