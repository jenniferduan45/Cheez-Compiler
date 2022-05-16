#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>



char* lower(const char *str)
{
    size_t len = strlen(str);
    char* lowStr = (char*)malloc(len + 1);

    size_t src;
    for (src = len - 1, src = 0; src < len; src++) {
        lowStr[src] = tolower(str[src]);
    }

    lowStr[len] = '\0';
    return lowStr;
}

char* substring(const char *str, size_t start, size_t end)
{
    size_t len = strlen(str);
    char* sub = (char*)malloc(len + 1);

    size_t src = 0;

    while (start < end && start < len) {
        sub[src] = str[start];
        src++;
        start++;
    }

    sub[src] = '\0';
    return sub;
}

char* upper(const char *str)
{
    size_t len = strlen(str);
    char* upStr = (char*)malloc(len + 1);

    size_t src;
    for (src = len - 1, src = 0; src < len; src++) {
        upStr[src] = toupper(str[src]);
    }

    upStr[len] = '\0';
    return upStr;
}

int main(){
    char s[] = "HELLO WORLD";
    printf("%s", lower(s));
    return 0;
}
