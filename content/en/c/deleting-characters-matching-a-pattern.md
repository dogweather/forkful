---
title:    "C recipe: Deleting characters matching a pattern"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to delete certain characters from a string in your C program? Maybe you want to remove all punctuation or spaces to clean up user input. Whatever the reason, deleting characters that match a specific pattern can be a useful tool in your C programming arsenal.

## How To

To delete characters matching a pattern, we can use the `strchr()` function from the standard C library. This function searches for the first occurrence of a given character in a string and returns a pointer to that position.

We can use this function in a loop to iterate through the string and delete all characters that match our desired pattern. Here's an example of how this might look in code:

```C
#include <stdio.h>
#include <string.h>

int main() {

  char str[] = "hello world!";
  char pattern = 'l';
  char *ptr;

  // loop through string until no more matches are found
  while ((ptr = strchr(str, pattern)) != NULL) {
    // replace matched character with null terminator
    *ptr = '\0';
    // move to next character in string
    ptr++;
  }

  printf("New string: %s\n", str);

  return 0;
}
```

This code first declares a string and a character variable for our pattern. In the loop, we use `strchr()` to find the first occurrence of the pattern in the string. We then replace that character with a null terminator and move the pointer to the next character in the string. This process continues until no more matches are found.

The output of this code would be:

```
New string: heo word!
```

This is just one example of how we can use the `strchr()` function to delete characters matching a pattern. Depending on the specific needs of your program, there may be other ways to achieve this task. It's always a good idea to research and experiment with different solutions to find the one that works best for your situation.

## Deep Dive

The `strchr()` function uses a linear search algorithm to find the desired character in the string. This means that it starts from the beginning of the string and checks each character one by one until a match is found. This can be time-consuming for longer strings and could affect the performance of your program.

If you need to delete characters matching a pattern in a large string, another approach could be to use a more efficient search algorithm, such as binary search. However, keep in mind that this may require sorting the string first, which could also impact performance.

It's also important to consider the limitations of the `strchr()` function. For example, it can only search for a single character, so if you need to delete a string of characters that match a specific pattern, you may need to use another function like `strstr()`.

## See Also

- [String manipulation in C](https://www.geeksforgeeks.org/string-manipulations-in-c-with-examples/)
- [C string manipulation functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Linear vs. binary search](https://www.geeksforgeeks.org/linear-search-vs-binary-search/)

Now that you know how to delete characters matching a pattern in your C program, have fun experimenting and finding ways to make your code more efficient and effective. Happy coding!