#ifndef USE_AFTER_FREE_H
#define USE_AFTER_FREE_H

#define MAX_USERS 16
#define USERNAME_LEN 16
#define PASSWORD_LEN 16

typedef struct {
    int uid;
    char username[USERNAME_LEN];
    char password[PASSWORD_LEN];
    int is_admin;
} User;

typedef struct {
    int uid;
    char username[USERNAME_LEN];
    int is_admin;
} Session;

extern User users[MAX_USERS];
extern int user_count;
extern Session* current_session;

void seed_admin(void);
void login(const char* username, const char* password);
void logout(void);
void perform_admin_action(void);
void add_user(const char* username, const char* password, int is_admin);
void cli_loop(void);

#endif
