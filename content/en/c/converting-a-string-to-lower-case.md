---
title:                "C recipe: Converting a string to lower case"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
If you're new to programming, you may not know what it means to "convert a string to lower case." In simple terms, it's a way to manipulate text in your code and make it lowercase instead of uppercase. This can be useful when dealing with user input or working with specific formatting requirements.

## How To
```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    // Declare a string variable
    char sentence[100];

    // Get input from the user
    printf("Enter a sentence: ");
    gets(sentence);

    // Convert the string to lower case
    for (int i = 0; i < strlen(sentence); i++)
    {
        sentence[i] = tolower(sentence[i]);
    }

    // Print the lower case string
    printf("Lower case sentence: %s", sentence);

    return 0;
}
```

**Sample Output:**

Enter a sentence: HELLO WORLD
Lower case sentence: hello world

In this example, we are using the C library functions `strlen()` and `tolower()` to convert the input string to lower case. The `strlen()` function is used to determine the length of the string, while the `tolower()` function converts each character to lowercase. We are also using a `for` loop to go through each character in the string.

## Deep Dive
Now that we have a basic understanding of how to convert a string to lower case, let's dive deeper into the topic.

First, it's important to note that C is a case-sensitive language. This means that uppercase and lowercase letters are treated as different characters. For example, `A` is not the same as `a` in the eyes of the C compiler.

When dealing with strings, C uses null-terminated strings, which means that each string ends with a special character called the null terminator (`\0`). This character is placed at the end of the string to indicate where it ends. When using string functions, it's important to ensure that the null terminator is not overwritten.

Another thing to keep in mind is that converting a string to lower case is not a one-size-fits-all solution. Different languages have different rules for converting strings to lower case. For example, in German, the letter `ß` is converted to `ss`, while in French, the letter `é` is converted to `e`. It's important to understand the specific language rules when working with strings.

## See Also
- [String Manipulation in C](https://www.programiz.com/c-programming/c-strings)
- [C Library - <ctype.h>](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)

By using the information and examples provided in this article, you should now have a better understanding of how to convert a string to lower case in C. Remember to always pay attention to the rules and specifics of the language you are working with, and you'll be able to manipulate strings with ease. Happy coding!