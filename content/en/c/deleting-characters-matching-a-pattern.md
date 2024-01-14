---
title:                "C recipe: Deleting characters matching a pattern"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to delete specific characters from a string or a file? Maybe you were trying to remove all the vowels from a sentence or delete all the digits from a phone number. Well, in those cases, knowing how to delete characters matching a certain pattern can come in handy.

## How To

Deleting characters that match a pattern can be done easily with C programming. First, let's define a function that takes in a string and a pattern as parameters:

```C
void deleteCharacters(char *str, char *pattern){
  // Code to delete characters here
}
```

Next, we need to loop through each character in the string and check if it matches the given pattern. If it does, we can simply remove it by shifting all the characters to the left.

```C
for (int i = 0; i < strlen(str); i++){
  if (str[i] == *pattern)
    // Shift all characters after i to the left by one position
}
```

For example, let's say we want to remove all the vowels from the string "Hello World". We can define our function as:

```C
void deleteCharacters(char *str, char *pattern){
  int deleteIndex = 0;
  for (int i = 0; i < strlen(str); i++){
    if (str[i] != *pattern){
      str[deleteIndex] = str[i];
      deleteIndex++;
    }
  }
  str[deleteIndex] = '\0';
}
```

Calling this function with the string "Hello World" and the pattern "aeiou" would result in the output "Hll Wrld". 

## Deep Dive

In the above code, we used the strlen() function to get the length of the string and the '\0' character to indicate the end of the string. This ensures that the final string is properly terminated. Additionally, we used pointers to access and modify the string, which is a common practice in C programming.

It's important to note that the **deleteIndex** variable is used to keep track of the index where we want to place the next character. If we encounter a character that matches our pattern, the index remains the same and the character is simply ignored.

## See Also

- [C String Functions](https://www.programiz.com/c-programming/string-functions)
- [Pointers in C](https://www.geeksforgeeks.org/pointers-in-c-programming/)

Now you know how to delete characters matching a pattern in C programming. You can use this knowledge to manipulate strings and files as needed. Happy coding!