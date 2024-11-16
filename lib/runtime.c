#include <stdio.h>
#include <stdlib.h>

void printInt(int value) {
    printf("%d\n", value);
}

void printString(const char *value) {
    printf("%s\n", value);
}

void error() {
    fprintf(stderr, "Runtime error!\n");
    exit(-1);
}

char* readString() {
    char * line = NULL;
    size_t size = 0;
    size_t len;
    len = getline(&line, &size, stdin);
    if (len > 0 && line[len-1] == '\n') line[len-1] = '\0';
    return line;
}

int readInt() {
    int value;
    if (scanf("%d", &value) != 1) {
        error();
    }
    getchar();
    return value;
}