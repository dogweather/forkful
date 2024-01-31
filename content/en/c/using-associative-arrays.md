---
title:                "Using associative arrays"
date:                  2024-01-30T18:57:27.487177-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using associative arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/using-associative-arrays.md"
---

{{< edit_this_page >}}

## What & Why?

Associative arrays, or hash maps, are key-value pairs that allow you to store and retrieve data with a key. They're incredibly useful in C because they enable faster data access compared to lists, especially when you're dealing with a large amount of data.

## How to:

C doesn't have built-in support for associative arrays like some other languages, but we can use structures and some library functions to get similar functionality. Here’s a simple implementation using the `uthash` library, which you'll need to include in your project.

First, define a structure to hold your key-value pairs:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // This will be our key
    char name[10]; // This is the value associated with our key
    UT_hash_handle hh; // Makes this structure hashable
} person;
```

Next, let's add some entries and retrieve them:

```C
int main() {
    person *my_people = NULL, *s;

    // Adding an entry
    s = (person*)malloc(sizeof(person));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(my_people, id, s);

    // Retrieving an entry
    int user_id = 1;
    HASH_FIND_INT(my_people, &user_id, s);
    if (s) {
        printf("Found: %s\n", s->name);
    }
    
    return 0;
}
```

Sample output would be:

```
Found: Alice
```

Don’t forget to free allocated memory and deallocate the hash table when done to avoid memory leaks.

## Deep Dive

While associative arrays aren't native to C, libraries like `uthash` fill the gap quite well, providing a fairly straightforward way to use this functionality. Historically, C developers had to implement their version of these data structures, leading to varied and often complex implementations, especially for those just beginning with the language.

Remember, the efficiency of using associative arrays in C greatly depends on how well the hash function distributes values across the table to minimize collisions. While libraries like `uthash` offer a good balance of ease-of-use and performance, in critical applications where performance is paramount, you might want to tailor or implement your own hash table.

For applications requiring maximum efficiency, alternative data structures or even other programming languages with built-in support for associative arrays might be a better choice. However, for many situations, especially where you're already working within a C environment, using a library like `uthash` provides a practical balance between performance and convenience.
