
/*
 * Tom Argaut - 260ADB185
 * Compile:
 * gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
 *
 * Resources:
 * ChatGPT 
 * Prompt: help to realise the TP Arithmetic Parser in C, with step-by-Step explanation of the Requests 
 * rendering history
 */
#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
/*First part of the TP creation of an input.txt and an output to try different ways of calculating then creation 
of a file with the running of the calc but before that try to go through the text, identifying calculation errors, 
line breaks, and other issues  */
#define FIRST_NAME "Tom"
#define LAST_NAME "Argaut"
#define STUDENT_ID "260ADB185"

#define PATH_SIZE 4096

/*chatgpt use: How can I represent both integer and floating-point values in C using a structure with implement of number 
parsing and arithmetic operations so that integer calculations remain integers while expressions involving decimal numbers use double*/

typedef struct {
    long long integer;
    double real;
    int is_float;
} Value;

/*chatgpt use : can you help me implement a recursive-descent parser in C for an arithmetic expression evaluator 
I need to support operator precedence, parentheses, and right-associative exponentiation using
and explain the purpose of each parsing function and provide code that follows these requirements*/

typedef struct {
    const char *text;
    size_t length;
    size_t index;
    size_t base;
    size_t error;
} Parser;

/* return the current position in the whole file for exemple */
static size_t position(Parser *p)
{
    return p->base + p->index + 1;
}

/* skip spaces and tabs between tokens but different modifications were needed even at the end to make it functional */
static void skip_spaces(Parser *p)
{
    while (p->index < p->length &&
           (p->text[p->index] == ' ' ||
            p->text[p->index] == '\t' ||
            p->text[p->index] == '\r')) {
        p->index++;
    }
}
/* keep the first error only */
static void set_error(Parser *p, size_t pos)
{
    if (p->error == 0) {
        p->error = pos;
    }
}

/* create an integer value */
static Value make_integer(long long n)
{
    Value v;
    v.integer = n;
    v.real = (double)n;
    v.is_float = 0;
    return v;
}

/* create a floating-point value as we have seing it  */
static Value make_float(double n)
{
    Value v;
    v.integer = 0;
    v.real = n;
    v.is_float = 1;
    return v;
}

/*
I keep track of the current index and where the line starts
so I can find the exact character position in the whole file
and positions start at 1 because this is what the subject asks for.
 */
static Value parse_expression(Parser *p);
static Value parse_product(Parser *p);
static Value parse_power(Parser *p);
static Value parse_primary(Parser *p);

/* read an integer or a decimal number */
static Value parse_number(Parser *p)
{
    size_t start = p->index;
    long long integer = 0;
    double real = 0.0;
    int has_dot = 0;

    while (p->index < p->length &&
           isdigit((unsigned char)p->text[p->index])) {
        integer = integer * 10 + (p->text[p->index] - '0');
        real = real * 10.0 + (p->text[p->index] - '0');
        p->index++;
    }

    if (p->index < p->length && p->text[p->index] == '.') {
        size_t dot = p->index;
        has_dot = 1;
        p->index++;

        if (p->index >= p->length ||
            !isdigit((unsigned char)p->text[p->index])) {
            set_error(p, p->base + dot + 1);
            return make_integer(0);
        }

        double place = 0.1;

        while (p->index < p->length &&
               isdigit((unsigned char)p->text[p->index])) {
            real += (p->text[p->index] - '0') * place;
            place *= 0.1;
            p->index++;
        }
    }

    if (p->index == start) {
        set_error(p, position(p));
        return make_integer(0);
    }

    if (has_dot) {
        return make_float(real);
    }

    return make_integer(integer);
}

/* read a number or a parenthesized expression without an error,i keep the first error because this is where the expression
first becomes invalid then the position depends on the error, for example the wrong
character or the start of the number after division. */
static Value parse_primary(Parser *p)
{
    Value value;

    skip_spaces(p);

    if (p->index >= p->length) {
        set_error(p, position(p));
        return make_integer(0);
    }

    if (p->text[p->index] == '(') {
        p->index++;
        value = parse_expression(p);
        skip_spaces(p);

        if (p->index >= p->length ||
            p->text[p->index] != ')') {
            set_error(p, position(p));
            return value;
        }

        p->index++;
        return value;
    }

    if (isdigit((unsigned char)p->text[p->index])) {
        return parse_number(p);
    }

    set_error(p, position(p));
    return make_integer(0);
}

/* calculate powers ** is right-associative, i use a different function for each operator level 
so that multiplication is done before addition. */
static Value parse_power(Parser *p)
{
    Value left = parse_primary(p);

    skip_spaces(p);

    if (p->index + 1 < p->length &&
        p->text[p->index] == '*' &&
        p->text[p->index + 1] == '*') {
        size_t op_pos = position(p);
        size_t exponent_start;
        Value right;
        double result;

        p->index += 2;
        skip_spaces(p);
        exponent_start = position(p);

        if (p->index < p->length && p->text[p->index] == '-') {
            set_error(p, exponent_start);
            return left;
        }

        right = parse_power(p);

        if (p->error != 0) {
            return left;
        }
        if ((!right.is_float && right.integer < 0) ||
            (right.is_float && right.real < 0.0)) {
            set_error(p, exponent_start);
            return left;
        }

        if (!left.is_float && !right.is_float) {
            long long result_integer = 1;
            long long exponent = right.integer;
            long long base = left.integer;

            while (exponent > 0) {
                result_integer *= base;
                exponent--;
            }

            return make_integer(result_integer);
        }

        result = pow(left.real, right.real);

        if (!isfinite(result)) {
            set_error(p, op_pos);
            return left;
        }

        return make_float(result);
    }

    return left;
}

/* calculate multiplication and division from left to right (rules of calculation)   */
static Value parse_product(Parser *p)
{
    Value left = parse_power(p);

    while (p->error == 0) {
        size_t op_pos;
        size_t right_start;
        char op;
        Value right;

        skip_spaces(p);

        if (p->index >= p->length ||
            (p->text[p->index] != '*' &&
             p->text[p->index] != '/')) {
            break;
        }

        if (p->index + 1 < p->length &&
            p->text[p->index] == '*' &&
            p->text[p->index + 1] == '*') {
            break;
        }

        op_pos = position(p);
        op = p->text[p->index];
        p->index++;

        skip_spaces(p);
        right_start = position(p);
        right = parse_power(p);

        if (p->error != 0) {
            break;
        }

        if (op == '/' &&
            ((!right.is_float && right.integer == 0) ||
             (right.is_float && right.real == 0.0))) {
            set_error(p, right_start);
            break;
        }

        if (!left.is_float && !right.is_float) {
            if (op == '*') {
                left = make_integer(left.integer * right.integer);
            } else {
                left = make_integer(left.integer / right.integer);
            }
        } else {
            double result;

            if (op == '*') {
                result = left.real * right.real;
            } else {
                result = left.real / right.real;
            }

            if (!isfinite(result)) {
                set_error(p, op_pos);
                break;
            }

            left = make_float(result);
        }
    }

    return left;
}

/* calculate addition and subtraction from left to right(same) i keep integers separate from floats so integer calculations
stay integers and division between integers gives an integer as if one of the values is a float, I use double for the result*/
static Value parse_expression(Parser *p)
{
    Value left = parse_product(p);

    while (p->error == 0) {
        size_t op_pos;
        char op;
        Value right;

        skip_spaces(p);

        if (p->index >= p->length ||
            (p->text[p->index] != '+' &&
             p->text[p->index] != '-')) {
            break;
        }

        op_pos = position(p);
        op = p->text[p->index];
        p->index++;

        right = parse_product(p);

        if (p->error != 0) {
            break;
        }

        if (!left.is_float && !right.is_float) {
            if (op == '+') {
                left = make_integer(left.integer + right.integer);
            } else {
                left = make_integer(left.integer - right.integer);
            }
        } else {
            double result;

            if (op == '+') {
                result = left.real + right.real;
            } else {
                result = left.real - right.real;
            }

            if (!isfinite(result)) {
                set_error(p, op_pos);
                break;
            }

            left = make_float(result);
        }
    }

    return left;
}

/* evaluate one line and check for extra characters */
static int evaluate_line(const char *text, size_t length,
                         size_t base, Value *result, size_t *error)
{
    Parser p;
    p.text = text;
    p.length = length;
    p.index = 0;
    p.base = base;
    p.error = 0;

    *result = parse_expression(&p);
    skip_spaces(&p);

    if (p.error == 0 && p.index < p.length) {
        set_error(&p, position(&p));
    }

    if (p.error != 0) {
        *error = p.error;
        return 0;
    }

    return 1;
}

/* chatgpt use:  show me how to read an entire input file in C when its size is unknown and may be really heavy
for exemple using malloc and realloc to grow a buffer dynamically 
but don't forget to handle memory allocation errors safely, and avoid imposing a fixed line-length limit.

read a whole file using a growing buffer i make the buffer bigger when it gets full instead of copying 
the file every time I read a character as chatgpt showed me for exemple doubling the size helps keep reading fast even for large files*/
static char *read_file(FILE *file, size_t *length)
{
    size_t capacity = 4096;
    size_t used = 0;
    char *buffer = malloc(capacity);
    int ch;

    if (buffer == NULL) {
        return NULL;
    }

    while ((ch = fgetc(file)) != EOF) {
        if (used == capacity) {
            size_t new_capacity = capacity * 2;
            char *new_buffer = realloc(buffer, new_capacity);

            if (new_buffer == NULL) {
                free(buffer);
                return NULL;
            }

            buffer = new_buffer;
            capacity = new_capacity;
        }

        buffer[used++] = (char)ch;
    }

    if (ferror(file)) {
        free(buffer);
        return NULL;
    }

    *length = used;
    return buffer;
}

/* Chapt use: i need to process an input file line by line and write results to a temporary file first but if any expression 
is invalid, the final output file should contain only ERROR: <position> instead of partial calculation result 
so how to implement this behavior and handle file errors

process a file without publishing partial results, i write the results to a temporary file first because a later
expression might contain an error so if that happens, I can write only the error to the final file instead of 
keeping results from expressions before the error */
static int process_file(const char *input_path,
                        const char *output_dir)
{
    FILE *input = fopen(input_path, "rb");
    FILE *temporary;
    FILE *output;
    char *buffer;
    size_t length;
    size_t start = 0;
    size_t error = 0;
    char base_name[PATH_SIZE];
    char output_path[PATH_SIZE];
    const char *name;
    size_t name_length;

    if (input == NULL) {
        fprintf(stderr, "Cannot open input file: %s\n", input_path);
        return 0;
    }

    buffer = read_file(input, &length);
    fclose(input);

    if (buffer == NULL) {
        fprintf(stderr, "Cannot read input file: %s\n", input_path);
        return 0;
    }

    temporary = tmpfile();

    if (temporary == NULL) {
        fprintf(stderr, "Cannot create temporary file\n");
        free(buffer);
        return 0;
    }

    while (start < length) {
        size_t end = start;
        size_t first;
        Value result;

        while (end < length && buffer[end] != '\n') {
            end++;
        }

        first = start;

        while (first < end &&
               (buffer[first] == ' ' || buffer[first] == '\t' ||
                buffer[first] == '\r')) {
            first++;
        }

        if (first < end && buffer[first] != '#') {
            if (!evaluate_line(buffer + start, end - start,
                               start, &result, &error)) {
                break;
            }

            if (result.is_float) {
                if (fprintf(temporary, "%.15g\n", result.real) < 0) {
                    error = 1;
                    break;
                }
            } else {
                if (fprintf(temporary, "%lld\n", result.integer) < 0) {
                    error = 1;
                    break;
                }
            }
        }

        if (end == length) {
            start = length;
        } else {
            start = end + 1;
        }
    }

    /* a final blank or comment line needs no result (the goal of going through all the items ) */
    if (error == 0 && length > 0) {

    }

    name = strrchr(input_path, '/');
    name = (name == NULL) ? input_path : name + 1;

    name_length = strlen(name);

    if (name_length >= sizeof(base_name)) {
        fprintf(stderr, "Input filename is too long\n");
        fclose(temporary);
        free(buffer);
        return 0;
    }

    strcpy(base_name, name);

    name_length = strlen(base_name);

    if (name_length >= 4 &&
        strcmp(base_name + name_length - 4, ".txt") == 0) {
        base_name[name_length - 4] = '\0';
    }

    if (snprintf(output_path, sizeof(output_path),
                 "%s/%s_%s_%s_%s.txt",
                 output_dir, base_name,
                 FIRST_NAME, LAST_NAME, STUDENT_ID)
        >= (int)sizeof(output_path)) {
        fprintf(stderr, "Output path is too long\n");
        fclose(temporary);
        free(buffer);
        return 0;
    }

    output = fopen(output_path, "w");

    if (output == NULL) {
        fprintf(stderr, "Cannot open output file: %s\n", output_path);
        fclose(temporary);
        free(buffer);
        return 0;
    }

    if (error != 0) {
        fprintf(output, "ERROR:%lu\n", (unsigned long)error);
    } else {
        int ch;

        rewind(temporary);

        while ((ch = fgetc(temporary)) != EOF) {
            if (fputc(ch, output) == EOF) {
                error = 1;
                break;
            }
        }
    }

    if (fclose(output) != 0) {
        fprintf(stderr, "Cannot write output file\n");
        fclose(temporary);
        free(buffer);
        return 0;
    }

    fclose(temporary);
    free(buffer);
    return 1;
}

/* return the filename without its directory path */
static const char *directory_name(const char *path)
{
    const char *name = strrchr(path, '/');
    return (name == NULL) ? path : name + 1;
}

/* create an output directory if it does not exist */
static int prepare_directory(const char *path)
{
    struct stat info;

    if (stat(path, &info) == 0) {
        if (S_ISDIR(info.st_mode)) {
            return 1;
        }

        fprintf(stderr, "Output path is not a directory\n");
        return 0;
    }

    if (mkdir(path, 0777) != 0) {
        fprintf(stderr, "Cannot create output directory: %s\n", path);
        return 0;
    }

    return 1;
}

/* Chatgpt use :  help me implement command-line options -d and -o in C? 
because my program should process every .txt file directly inside a directory, ignore subdirectories and non-text files, 
create the output directory if necessary, and generate output filenames but use the permitted standard and POSIX headers and explain
how this works

Process every .txt file directly inside a directory */
static int process_directory(const char *directory,
                             const char *output_dir)
{
    DIR *dir = opendir(directory);
    struct dirent *entry;
    int success = 1;

    if (dir == NULL) {
        fprintf(stderr, "Cannot open directory: %s\n", directory);
        return 0;
    }

    while ((entry = readdir(dir)) != NULL) {
        size_t n = strlen(entry->d_name);
        char input_path[PATH_SIZE];
        struct stat info;

        if (n < 4 || strcmp(entry->d_name + n - 4, ".txt") != 0) {
            continue;
        }

        if (snprintf(input_path, sizeof(input_path), "%s/%s",
                     directory, entry->d_name)
            >= (int)sizeof(input_path)) {
            fprintf(stderr, "Input path is too long\n");
            success = 0;
            continue;
        }

        if (stat(input_path, &info) != 0 || !S_ISREG(info.st_mode)) {
            continue;
        }

        if (!process_file(input_path, output_dir)) {
            success = 0;
        }
    }

    closedir(dir);
    return success;
}
/* the main was one of the most difficult task because all need to combine together and i had a lot of problems 
like at the begining a lot of errors so i modified main to handle command-line arguments such as -d and -o and reject invalid options
i added checks to ensure the user provides a valid input file or directory
i implemented default output directory naming and created the directory when necessary
Finally, i made main choose between processing a single file or all .txt files in a directory, and return the appropriate exit status*/

int main(int argc, char *argv[])
{
    const char *input_file = NULL;
    const char *input_dir = NULL;
    const char *output_option = NULL;
    char default_dir[PATH_SIZE];
    char output_dir[PATH_SIZE];
    int i;
    int success;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 ||
            strcmp(argv[i], "--dir") == 0) {
            if (i + 1 >= argc || input_dir != NULL) {
                fprintf(stderr, "Invalid directory option\n");
                return 1;
            }
            input_dir = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 ||
                   strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 >= argc || output_option != NULL) {
                fprintf(stderr, "Invalid output option\n");
                return 1;
            }
            output_option = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        } else if (input_file == NULL) {
            input_file = argv[i];
        } else {
            fprintf(stderr, "Too many input files\n");
            return 1;
        }
    }

    if ((input_dir == NULL && input_file == NULL) ||
        (input_dir != NULL && input_file != NULL)) {
        fprintf(stderr,
                "Usage: calc [-d DIR] [-o OUTDIR] [input.txt]\n");
        return 1;
    }

    if (output_option != NULL) {
        if (strlen(output_option) >= sizeof(output_dir)) {
            fprintf(stderr, "Output directory path is too long\n");
            return 1;
        }
        strcpy(output_dir, output_option);
    } else {
        const char *base = directory_name(
            (input_dir != NULL) ? input_dir : input_file);
        char input_base[PATH_SIZE];

        if (strlen(base) >= sizeof(input_base)) {
            fprintf(stderr, "Input path is too long\n");
            return 1;
        }

        strcpy(input_base, base);

        if (input_dir == NULL) {
            size_t n = strlen(input_base);

            if (n >= 4 &&
                strcmp(input_base + n - 4, ".txt") == 0) {
                input_base[n - 4] = '\0';
            }
        }

        if (snprintf(default_dir, sizeof(default_dir),
                     "%s_%s_%s", input_base,
                     FIRST_NAME, STUDENT_ID)
            >= (int)sizeof(default_dir)) {
            fprintf(stderr, "Default directory path is too long\n");
            return 1;
        }

        strcpy(output_dir, default_dir);
    }

    if (!prepare_directory(output_dir)) {
        return 1;
    }

    if (input_dir != NULL) {
        success = process_directory(input_dir, output_dir);
    } else {
        success = process_file(input_file, output_dir);
    }

    return success ? 0 : 1;
}
