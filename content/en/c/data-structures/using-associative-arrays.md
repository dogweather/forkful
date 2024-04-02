---
date: 2024-02-03 17:50:13.746655-07:00
description: "Associative arrays, known in other languages as maps or dictionaries,\
  \ are key-value pairs used for efficient data lookup and manipulation. Unlike\u2026"
lastmod: '2024-03-13T22:45:00.505217-06:00'
model: gpt-4-0125-preview
summary: "Associative arrays, known in other languages as maps or dictionaries, are\
  \ key-value pairs used for efficient data lookup and manipulation. Unlike\u2026"
title: Using associative arrays
weight: 15
---

## What & Why?

Associative arrays, known in other languages as maps or dictionaries, are key-value pairs used for efficient data lookup and manipulation. Unlike traditional arrays that use integer indexes, associative arrays use keys, making data access more intuitive and flexible for programmers.

## How to:

C does not have built-in support for associative arrays like some higher-level languages, but you can simulate them using structures and hashing. Below is a simplistic example using a combination of a struct and a simple hashing function to implement an associative array for storing and accessing integers by string keys.

First, define a structure to represent a single key-value pair and another to represent the associative array itself:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePair;

typedef struct {
    KeyValuePair* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int value) {
    unsigned int slot = hash(key);

    KeyValuePair* item = (KeyValuePair*)malloc(sizeof(KeyValuePair));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssocArray* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssocArray a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // Output: 1
    printf("%d\n", find(&a, "key2")); // Output: 2

    return 0;
}
```

This example demonstrates basic operations: initializing an associative array, inserting key-value pairs, and finding values by keys. Note that this code lacks collision handling and is meant for educational purposes.

## Deep Dive

The concept of associative arrays predates C, but the language's low-level nature does not directly support them as built-in types. This encourages a deeper understanding of data structures and algorithms, including hashing mechanisms for efficient key-value mapping. Many C libraries and frameworks offer more sophisticated approaches for implementing associative arrays, such as GLib's `GHashTable`, which provides a robust implementation complete with collision handling, dynamic resizing, and support for arbitrary key and value types. 

While the manual construction of associative arrays in C can be seen as cumbersome compared to languages with built-in support, it offers invaluable insights into the inner workings of data structures, sharpening a programmer’s skills in problem-solving and optimization. However, for production code or more complex applications, leveraging existing libraries like GLib is often a more practical and time-efficient approach.
