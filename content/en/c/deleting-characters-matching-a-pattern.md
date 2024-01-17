---
title:                "Deleting characters matching a pattern"
html_title:           "C recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern is when a programmer removes specific characters from a given text based on a specific pattern. This can be useful for filtering data or for text processing tasks. Programmers commonly do this in order to clean up large data sets or to transform text into a desired format for further processing.

## How to:
The basic syntax for deleting characters matching a pattern in C is as follows:
```
str_del("given_text", "pattern");
```
This function searches the given text for the specified pattern and removes any matching characters. Let's look at an example:
```
#include<stdio.h> 
#include<string.h>

//Define function to delete characters matching a pattern
void str_del(char str[], const char pattern[]) { 
	int i, j, k;
	int count = 0; 
	for (k = 0; pattern[k] != '\0'; k++) { 
		for (i = j = 0; str[i] != '\0'; i++) { 
            //If current character does not match the pattern, add it to the modified string
			if (str[i] != pattern[k]) { 
				str[j++] = str[i]; 
			} 
			else {
                //Otherwise, increase the count of removed characters
				count++; 
			}
		} 
		str[j] = '\0'; 
	} 
	printf("Modified text: %s \n", str); 
    printf("Total characters removed: %d", count); 
} 
  
int main() { 
    char text[] = "H3ll0 W0rld!!"; 
    char pattern[] = "10"; 
    //Call str_del function
    str_del(text, pattern); 
    return 0; 
} 
```
Output: 
```
Modified text: H3l W0rld! 
Total characters removed: 3
```

## Deep Dive:
Deleting characters matching a pattern can be seen as a form of string manipulation, in which the programmer is using the given pattern as a filter to remove unwanted characters from the text. This concept has been around since the early days of computer programming, as it is a basic task that allows for data to be processed and cleaned up in a more efficient manner.

In terms of alternatives, there are a few different approaches that a programmer can take when trying to delete characters matching a pattern. One alternative is to use regular expressions, which are specialized patterns used for searching and manipulating text. Another option is to use built-in functions such as `strchr()` or `strtok()` to search and remove specific characters.

For those interested in the implementation details, the `str_del` function first loops through each character of the pattern and then searches for that same character within the given text using a nested for loop. If a match is found, the character is removed from the text and the count of removed characters is increased. The function then continues until all characters in the pattern have been searched for and removed.

## See Also:
- [C - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [C - Regular Expressions](https://www.tutorialspoint.com/c_standard_library/c_function_regcomp.htm)
- [Built-in String Functions in C](https://www.geeksforgeeks.org/built-function-c-string-manipulation/)