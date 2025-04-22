#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char *buffer = (char *)malloc(10);
    strcpy(buffer, "Hello");
    printf("Buffer content: %s\n", buffer);

    free(buffer);

    printf("After free: %s\n", buffer);
    strcpy(buffer, "World"); // Potentially dangerous write to freed memory

    return 0;
}
