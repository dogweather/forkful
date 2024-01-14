---
title:                "C recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to extract a specific portion of a string in your C program? Maybe you wanted to separate a word from a sentence, or extract a specific date from a longer string. This is where extracting substrings comes in handy. It allows you to manipulate strings in a more precise and efficient way, making your code more dynamic and robust.

## How To 

To start off, let's define a string that we want to extract a substring from:

```C 
char str[] = "Hello World";
```

Now, let's say we want to extract the word "World" from this string. We can do this by using the `strncpy()` function:

```C 
char newStr[6]; // This will store our extracted substring 
strncpy(newStr, str+6, 5); // The "+6" indicates where to start extracting, and "5" is the length of the substring we want 
printf("Extracted substring: %s", newStr); // Output: World
```

In this example, we used the `strncpy()` function to copy a portion of the original string into a new string. The first argument is the destination, the second argument is the source (in this case, we use array indexing to specify the starting point of the substring), and the third argument is the length of the substring. 

Another useful function for extracting substrings is `strtok()`, which stands for "string tokenizer". This function allows you to split a string into smaller strings based on a delimiter. For example:

```C 
char str[] = "John,Smith,26";
char *token; // This will store each token (or smaller string) 
token = strtok(str, ","); // Split the string based on the "," delimiter 
printf("First name: %s\n", token); // Output: John 
token = strtok(NULL, ","); // Use NULL as the first argument to continue splitting the string 
printf("Last name: %s\n", token); // Output: Smith 
token = strtok(NULL, ","); // Keep splitting until there are no more tokens 
printf("Age: %s\n", token); // Output: 26
```

## Deep Dive 

Now, let's take a closer look at how these functions actually work. When you extract a substring using `strncpy()`, the function creates a new string and copies the specified portion of the original string into it. This means that the original string remains unchanged. It's important to note that when using this function, you need to make sure that the destination string has enough space to store the substring, otherwise you may encounter buffer overflows.

As for `strtok()`, this function works by modifying the original string. When it encounters the delimiter, it replaces it with a null character and returns a pointer to the tokenized string. However, this also means that the original string is permanently altered. To avoid this, you can create a copy of the string and use that instead.

## See Also 

- [C string functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [String manipulation in C](https://www.cs.usfca.edu/~parrt/course/601/lectures/stringsimplification/index.html)
- [Manipulating strings in C](https://www.programiz.com/c-programming/c-strings)

Happy coding!