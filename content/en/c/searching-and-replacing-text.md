---
title:                "Searching and replacing text"
html_title:           "C recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

If you've ever found yourself faced with a large chunk of text that needs to be edited, you know how time-consuming and error-prone it can be to manually search and replace every instance of a specific word or phrase. That's where the power of searching and replacing text using C programming comes in.

## How To 

To start, you'll need to define a string variable that will hold the text you want to search and replace. Let's say we have a sentence like "I love coding in C" and we want to replace "love" with "enjoy". We'll set up our string variable as follows:

```C
char str[20] = "I love coding in C";
```

Next, we'll use the `strchr()` function to locate the first occurrence of the word we want to replace. This function takes in two arguments - the string to search and the character to find. In this case, we'll search for the letter "l" in our string variable.

```C
char *ptr = strchr(str, 'l');
```

Now, we can use the `strcpy()` function to replace the word "love" with "enjoy". This function takes in two arguments - the destination string and the source string. We'll set our destination string to the location of the "l" we just found and our source string to "enjoy".

```C
strcpy(ptr, "enjoy");
```

Finally, we can print our updated string variable to see the results:

```C
printf("%s", str);
// Output: "I enjoy coding in C"
```

## Deep Dive

There are a few things to keep in mind when using searching and replacing techniques in C programming. First, the `strchr()` function only searches for single-character matches. If you want to search for a whole word or phrase, you can use the `strstr()` function instead, which takes in two arguments - the string to search and the substring to find.

Additionally, the `strcpy()` function only replaces the specified word or phrase with the same number of characters. In our example, "love" and "enjoy" are both four-letter words, so they can be easily replaced. However, if the replacement string is longer than the original string, it may result in overwriting important data in the memory.

To avoid these issues, consider using the `memmove()` function instead, which can handle copying and moving strings with different lengths.

## See Also

- [strchr() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [strstr() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [strcpy() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strcpy.htm)
- [memmove() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_memmove.htm)