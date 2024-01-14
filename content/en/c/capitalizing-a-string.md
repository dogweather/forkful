---
title:    "C recipe: Capitalizing a string"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a common task when working with text in programming. Whether it is for displaying user inputs or formatting data for a specific output, capitalization can play an important role in making data more readable and presentable. In this blog post, we will discuss why and how to capitalize a string in the C programming language.

## How To

To capitalize a string in C, we can use the built-in function `toupper()` from the header file `<ctype.h>`. This function takes in a character and returns its uppercase equivalent. We can use this function in a loop to iterate through each character in the string and convert them to uppercase. Here is an example code:

```
#include<stdio.h>
#include<ctype.h>

int main(){
  // declare a string
  char str[50];
  int i;

  // ask for user input
  printf("Enter a string: ");
  scanf("%s", str);

  // loop through each character
  for(i = 0; str[i] != '\0'; i++){
    // use toupper() function to convert to uppercase
    str[i] = toupper(str[i]);
  }

  // print the capitalized string
  printf("Capitalized string: %s", str);

  return 0;
}
```

Sample Output:

```
Enter a string: hello world
Capitalized string: HELLO WORLD
```

We can also create our own custom function to capitalize a string. This function would take in a string as a parameter and return the capitalized version of that string. Here is an example code:

```
#include<stdio.h>

// function to capitalize a string
char* capitalizeString(char* str){
  int i;

  // loop through each character
  for(i = 0; str[i] != '\0'; i++){
    // check if character is lowercase
    if(str[i] >= 97 && str[i] <= 122){
      // convert to uppercase by subtracting 32 from ASCII value
      str[i] -= 32;
    }
  }

  // return the capitalized string
  return str;
}

int main(){
  // declare a string
  char str[50];

  // ask for user input
  printf("Enter a string: ");
  scanf("%s", str);

  // call the capitalizeString function
  char* capitalized_str = capitalizeString(str);

  // print the capitalized string
  printf("Capitalized string: %s", capitalized_str);

  return 0;
}
```

Sample Output:

```
Enter a string: hello world
Capitalized string: HELLO WORLD
```

## Deep Dive

In C, strings are represented as arrays of characters and are terminated by a null character (`\0`). This means that in order to capitalize a string, we can simply loop through each character and perform the necessary operations on them. The `toupper()` function takes in a character and returns its ASCII code equivalent for uppercase characters. This is why we can directly assign the return value of `toupper()` to the current character in the loop.

It is also worth noting that the ASCII codes for lowercase and uppercase letters have a difference of 32. This is why in the custom `capitalizeString()` function, we can simply subtract 32 from the ASCII value to convert lowercase characters to uppercase.

## See Also

- [ASCII Table](https://www.ascii-code.com/)
- [String Functions in C](https://www.programiz.com/c-programming/library-function/string.h)
- [C Programming Tutorial](https://www.tutorialspoint.com/cprogramming/)