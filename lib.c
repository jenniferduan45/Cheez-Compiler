#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>





// define a node
typedef struct Node {
    char* id;
    void* data;
    struct Node* next;
} Node;


// gen id
char* random_id(int32_t length) {
    char* dest = (char*) malloc(sizeof(char) * (length + 1));
    char charset[] = "0123456789"
                     "abcdefghijklmnopqrstuvwxyz"
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int32_t i = 0;
    while (i < length) {
        int32_t index = (double) rand() / RAND_MAX * (sizeof charset - 1);
        dest[i] = charset[index];
        i++;
    }
    dest[length] = '\0';
    return dest;
}





// setup a node
Node* new_node(void* data) {
    Node* n =  (Node *) malloc(sizeof(Node));
    n->id = random_id(8);
    n->data = data;
    n->next = NULL;
    return n;
}




