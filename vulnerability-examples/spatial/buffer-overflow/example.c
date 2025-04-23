#include <string.h>
#include <stdio.h>
#include <stdint.h>

void some_useful_function(char *input) {
    char buffer[16];
    printf("Address of buffer: %p\n", (void *)buffer);
    strcpy(buffer, input);
    printf("Buffer content: %s\n", buffer);
}

int main() {
    some_useful_function("This string value is much more longer than the declared buffer size!");
    return 0;
}
