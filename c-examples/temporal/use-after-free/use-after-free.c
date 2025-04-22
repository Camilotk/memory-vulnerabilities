#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "use-after-free.h"

User users[MAX_USERS];
int user_count = 0;
Session* current_session = NULL;

void seed_admin(void) {
    strcpy(users[0].username, "admin");
    strcpy(users[0].password, "admin123");
    users[0].uid = 0;
    users[0].is_admin = 1;
    user_count = 1;
}

void login(const char* username, const char* password) {
    for (int i = 0; i < user_count; i++) {
        if (strncmp(users[i].username, username, USERNAME_LEN) == 0 &&
            strncmp(users[i].password, password, PASSWORD_LEN) == 0) {

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

void logout(void) {
    if (current_session) {
        printf("[*] Logging out '%s'\n", current_session->username);
        free(current_session);
    } else {
        printf("[!] No user is logged in\n");
    }
}

void perform_admin_action(void) {
    if (!current_session || current_session->is_admin != 1) {
        printf("[-] Access denied: admin only\n");
        return;
    }

    printf("[+] Admin action performed by '%s'\n", current_session->username);
}

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

    if (current_session) {
        free(current_session);
    }
}

int main() {
    seed_admin();
    cli_loop();
    return 0;
}
