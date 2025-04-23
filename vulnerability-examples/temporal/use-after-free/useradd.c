/**
 * @file use-after-free.c
 * @brief Demonstration of use-after-free vulnerability in C
 * 
 * This program illustrates a classic use-after-free (UAF) vulnerability through
 * a simple user management system. It demonstrates how improper memory management
 * can lead to security vulnerabilities that enable unauthorized access and 
 * potential memory-related exploits.
 * 
 * Vulnerability: The program allocates a session memory dynamically and frees it
 * without setting the pointer to NULL. Subsequent attempts to access the freed
 * memory can lead to undefined behavior, potentially allowing an attacker to
 * read sensitive information or execute malicious code.
 * 
 * @author Camilo de Azevedo <camilotk@gmail.com>
 * @date April 22, 2025
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "useradd.h"

/** Global array to store user information */
User users[MAX_USERS];
int user_count = 0;

/** 
 * Global pointer to track the current user session
 * VULNERABILITY: Pointer remains valid after being freed
 */
Session* current_session = NULL;

/**
 * @brief Initialize the system with a default admin user
 * 
 * Creates the first user in the system with admin privileges.
 */
void seed_admin(void) {
    strcpy(users[0].username, "admin");
    strcpy(users[0].password, "admin123");
    users[0].uid = 0;
    users[0].is_admin = 1;
    user_count = 1;
}

/**
 * @brief Attempt to log in a user
 * 
 * VULNERABILITY: This function demonstrates use-after-free risks:
 * - Dynamically allocates session memory
 * - Does not check for existing session
 * - Leaves pointer potentially accessible after free()
 * 
 * @param username User's username to authenticate
 * @param password User's password for authentication
 */
void login(const char* username, const char* password) {
    for (int i = 0; i < user_count; i++) {
        if (strncmp(users[i].username, username, USERNAME_LEN) == 0 &&
            strncmp(users[i].password, password, PASSWORD_LEN) == 0) {

            /* VULNERABLE: Potential use-after-free scenario */
            current_session = malloc(sizeof(Session));
            current_session->uid = users[i].uid;
            strncpy(current_session->username, users[i].username, USERNAME_LEN - 1);
            current_session->username[USERNAME_LEN - 1] = '\0';
            current_session->is_admin = users[i].is_admin;

            printf("[+] Logged in as '%s' (admin=%d)\n", current_session->username, current_session->is_admin);
            return;
        }
    }
    printf("[-] Login failed: invalid credentials\n");
}

/**
 * @brief Log out the current user
 * 
 * VULNERABILITY: 
 * Frees the session memory but does not invalidate the pointer,
 * allowing potential use-after-free exploitation.
 */
void logout(void) {
    if (current_session) {
        printf("[*] Logging out '%s'\n", current_session->username);
        
        /* freed without nullifying pointer */
        free(current_session);
    } else {
        printf("[!] No user is logged in\n");
    }
}

/**
 * @brief Perform an admin-only action
 */
void perform_admin_action(void) {
    if (!current_session || current_session->is_admin != 1) {
        printf("[-] Access denied: admin only\n");
        return;
    }

    printf("[+] Admin action performed by '%s'\n", current_session->username);
}

/**
 * @brief Add a new user to the system
 * 
 * @param username New user's username
 * @param password New user's password
 * @param is_admin Flag to set admin privileges
 */
void add_user(const char* username, const char* password, int is_admin) {
    if (!current_session || current_session->is_admin != 1) {
        printf("[-] Only admins can add users\n");
        return;
    }

    if (user_count >= MAX_USERS) {
        printf("[-] User limit reached\n");
        return;
    }

    User* u = &users[user_count++];
    u->uid = user_count;
    strncpy(u->username, username, USERNAME_LEN - 1);
    u->username[USERNAME_LEN - 1] = '\0';
    strncpy(u->password, password, PASSWORD_LEN - 1);
    u->password[PASSWORD_LEN - 1] = '\0';
    u->is_admin = is_admin;

    printf("[+] Added user '%s' (admin=%d)\n", u->username, u->is_admin);
}

/**
 * @brief Main command-line interface loop
 */
void cli_loop(void) {
    char command[32];
    char username[32];
    char password[32];
    int is_admin;

    printf("Useradd-like CLI (type 'help')\n");

    while (1) {
        printf("> ");
        if (scanf("%31s", command) != 1) break;

        if (strcmp(command, "login") == 0) {
            scanf("%15s %15s", username, password);
            login(username, password);
        } else if (strcmp(command, "logout") == 0) {
            logout();
        } else if (strcmp(command, "admin") == 0) {
            perform_admin_action();
        } else if (strcmp(command, "adduser") == 0) {
            scanf("%15s %15s %d", username, password, &is_admin);
            add_user(username, password, is_admin);
        } else if (strcmp(command, "help") == 0) {
            printf("Commands:\n");
            printf("  login <username> <password>\n");
            printf("  logout\n");
            printf("  admin\n");
            printf("  adduser <username> <password> <is_admin>\n");
            printf("  exit\n");
        } else if (strcmp(command, "exit") == 0) {
            break;
        } else {
            printf("[!] Unknown command\n");
        }
    }

    /* Potential use-after-free */
    if (current_session) {
        free(current_session);
    }
}

/**
 * Main function - entry point of the program
 */
int main(void) {
    seed_admin();
    cli_loop();
    return 0;
}
