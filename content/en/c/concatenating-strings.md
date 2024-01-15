---
title:                "Concatenating strings"
html_title:           "C recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a fundamental operation in programming, especially in languages like C that do not have built-in string manipulation functions. By combining strings, we can generate new strings that contain the desired information, allowing us to manipulate and display data in a meaningful way.

## How To

To concatenate strings in C, we use the `strcat()` function, which stands for "string concatenation." This function takes in two strings as parameters and appends the second string to the end of the first string.

```
#include <stdio.h>
#include <string.h>

int main() {

  // Declare and initialize two strings
  char greeting[20] = "Hello ";
  char name[10] = "John";

  // Concatenate the two strings
  strcat(greeting, name);

  // Print the result
  printf("%s", greeting);

  return 0;
}

// Output: Hello John
```

In the above example, we first declare and initialize two character arrays, `greeting` and `name`, with the values "Hello " and "John" respectively. We then use the `strcat()` function to combine these two strings, resulting in the output "Hello John."

It's important to note that the first string must have enough space to hold the concatenated result. In the example above, `greeting` is declared with a size of 20, which is enough to hold "Hello " and "John" together. If the first string is not large enough, it can lead to unexpected behavior and even errors in your program.

## Deep Dive

Under the hood, the `strcat()` function works by finding the null character (`'\0'`) at the end of the first string and overwriting it with the characters from the second string. It then adds a new null character at the end, marking the end of the concatenated string.

One thing to keep in mind is that `strcat()` does not perform any size checks. It assumes that the first string is large enough to accommodate the concatenated result, so it's important to be mindful of the sizes of your strings when using this function.

Another important aspect to keep in mind is that the two strings being concatenated must be contiguous in memory. If they are not, the behavior is undefined and can lead to errors or unexpected results. This is why it is recommended to always use `strcat()` with character arrays rather than pointers.

## See Also

- [String Concatenation in C](https://www.geeksforgeeks.org/string-concatenation-in-c/)
- [C Standard Library - strcat()](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Introduction to C Programming/String Operations](https://en.wikibooks.org/wiki/C_Programming/String_operations)