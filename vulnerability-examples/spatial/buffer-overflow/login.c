/**
 * @file buffer_overflow.c
 * @brief Demonstration of buffer overflow vulnerability in C
 * 
 * This program illustrates a classic buffer overflow vulnerability through a simple
 * authentication system. It demonstrates how inadequate bounds checking can lead
 * to security vulnerabilities that enable bypassing authentication mechanisms.
 * 
 * Vulnerability: The program allocates a fixed-size buffer for password input but uses
 * an unsafe function (gets()) that does not perform bounds checking, potentially
 * allowing an attacker to overflow the buffer and overwrite adjacent memory.
 * 
 * @author Camilo Cunha de Azevedo <camilotk@gmail.com>
 * @date April 20, 2025
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "login.h"

/**
 * @brief Executes a privileged shell (for demonstration purposes)
 * 
 * This function represents the "restricted" functionality that
 * should only be accessible after successful authentication.
 * In a real system, this might be administrative functions or
 * access to sensitive data.
 */
void privileged_access(void) {
    printf("\nLaunching privileged shell...\n\n");
    // Use a more reliable method to spawn a shell
    char *argv[] = {"/bin/sh", NULL};
    char *envp[] = {NULL};
    execve("/bin/sh", argv, envp);
    
    // This only runs if execve fails
    perror("execve failed");
    exit(1);
}

/**
 * @brief Validates the provided password against the stored password
 * 
 * @param password The password string to validate
 * @return int 1 if the password is valid, 0 otherwise
 */
int validate_password(char *password) {
    char stored_password[64];
    int fd;
    ssize_t bytes_read;
    
    /* Open the password file */
    fd = open("./password.txt", O_RDONLY);
    if (fd < 0) {
        perror("Error opening password file");
        return 0;
    }
    
    /* Read the stored password */
    bytes_read = read(fd, stored_password, sizeof(stored_password) - 1);
    close(fd);
    
    if (bytes_read <= 0) {
        fprintf(stderr, "Error reading password file\n");
        return 0;
    }
    
    /* Null-terminate the string and remove trailing newline if present */
    stored_password[bytes_read] = '\0';
    if (bytes_read > 0 && stored_password[bytes_read - 1] == '\n') {
        stored_password[bytes_read - 1] = '\0';
    }
    
    /* Compare the provided password with the stored password */
    return (strcmp(stored_password, password) == 0);
}

/**
 * @brief Handles user authentication with a password prompt
 * 
 * VULNERABILITY: This function contains a buffer overflow vulnerability.
 * It allocates a fixed-size buffer (16 bytes) for the password but uses 
 * gets(), which does not perform bounds checking. If the input exceeds
 * 16 bytes, it will overflow the buffer and potentially overwrite the
 * return address on the stack.
 * 
 * @return int Authentication status (1 for success, 0 for failure)
 */
int authenticate_user(void) {
    /* 
     * VULNERABLE CODE: Fixed-size buffer with unsafe input function
     * Only 16 bytes allocated, but gets() has no bounds checking
     */
    char password[16];
    
    printf("Enter password: ");
    
    /* 
     * VULNERABILITY: gets() reads until newline with no bounds checking
     * This allows writing beyond the end of the buffer
     */
    gets(password);  // WARNING: gets() is deprecated and unsafe
    
    /* Validate the password */
    return validate_password(password);
}

/**
 * Main function - demonstrates the buffer overflow vulnerability
 */
int main(void) {
    int authenticated = 0;
    
    printf("SECURE SYSTEM ACCESS\n");
    printf("-------------------\n\n");
    
    /* Attempt authentication */
    authenticated = authenticate_user();
    
    if (authenticated) {
        printf("Authentication successful!\n");
        privileged_access();
    } else {
        printf("Authentication failed. Access denied.\n");
    }
    
    return 0;
}
