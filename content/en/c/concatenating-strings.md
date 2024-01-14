---
title:                "C recipe: Concatenating strings"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to combine multiple pieces of text into one string? Concatenation in C programming allows you to do just that! By combining strings, you can create longer, more dynamic messages or data.

## How To

To concatenate strings in C, you'll need to use the `strcat()` function from the standard library `string.h`. This function takes two strings and combines them together, storing the result in the first string. Let's take a look at an example:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char name[20] = "John";
    char greeting[20] = "Hello ";

    strcat(greeting, name);

    printf("%s\n", greeting);
    // Output: Hello John

    return 0;
}
```

In the above code, we have two strings: `name` and `greeting`. We use the `strcat()` function to concatenate `name` onto the end of `greeting`. When we print `greeting`, we see the result of the two strings combined.

You can also concatenate multiple strings in a single line by using the `+` operator. Let's see another example:

```C
#include <stdio.h>

int main() {
    char word1[10] = "Hello ";
    char word2[10] = "World";
    char word3[10] = "!";

    printf("%s\n", word1 + word2 + word3);
    // Output: Hello World!

    return 0;
}
```

In this code, we are using the `+` operator to combine three strings, `word1`, `word2`, and `word3`. Since the `printf()` function uses the `%s` format specifier, we can directly concatenate the strings without calling a separate function.

## Deep Dive 

Concatenation can also be done using the `sprintf()` function. This function takes a destination string, a format string, and any number of arguments to insert into the format string. Let's see an example:

```C
#include <stdio.h>

int main() {
    char output[25];
    char name[10] = "John";
    char age[3] = "26";

    sprintf(output, "My name is %s and I am %s years old.", name, age);

    printf("%s\n", output);
    // Output: My name is John and I am 26 years old.

    return 0;
}
```

In this code, we use the `sprintf()` function to concatenate the `name` and `age` variables into the `output` string. This allows us to create a more dynamic message by inserting variables into the format string.

It's important to note that the `strcat()` and `sprintf()` functions both require enough space in the destination string to hold the combined string. If the destination string is not large enough, the program may crash or unexpected behavior may occur.

## See Also 

Here are some additional resources for learning about string concatenation: 

- [C String Concatenation - GeeksforGeeks](https://www.geeksforgeeks.org/string-concatenation-in-c/)
- [C Programming Tutorial - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [String Concatenation in C - Programiz](https://www.programiz.com/c-programming/c-strings)

Now that you know how to concatenate strings in C, you can use this powerful feature to create more dynamic and flexible programs!