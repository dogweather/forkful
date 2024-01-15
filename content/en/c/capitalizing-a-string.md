---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Why
Capitalizing a string in C may seem like a small task, but it can have a big impact on the readability and organization of your code. By capitalizing strings, you can easily differentiate between different types of data and make your code more user-friendly.

# How To
To capitalize a string in C, there are a few different methods you can use. Let's take a look at some examples and sample output to see how it all works.

```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "hello world"; // create a string to capitalize
    char capitalStr[12]; // create a new string to hold the capitalized version
    int i;

    // method 1: using the toupper() function
    for(i = 0; str[i] != '\0'; i++) // loop through each character of the string
    {
        capitalStr[i] = toupper(str[i]); // capitalize each character using toupper()
    }
    capitalStr[i] = '\0'; // add null terminator to the end of the string
    printf("Capitalized string: %s\n", capitalStr);

    // method 2: using the strupr() function
    strcpy(capitalStr, str); // copy original string into new string
    strupr(capitalStr); // use strupr() to convert all characters to uppercase
    printf("Capitalized string: %s\n", capitalStr);
    
    // method 3: manually changing the ASCII value of characters
    for(i = 0; str[i] != '\0'; i++) // loop through each character of the string
    {
        if(str[i] >= 'a' && str[i] <= 'z') // check if character is lowercase
        {
            capitalStr[i] = str[i] - 32; // convert to uppercase by subtracting 32 from ASCII value
        }
        else
        {
            capitalStr[i] = str[i]; // keep uppercase characters the same
        }
    }
    capitalStr[i] = '\0'; // add null terminator to the end of the string
    printf("Capitalized string: %s\n", capitalStr);

    return 0;
}
```
**Output:**
```
Capitalized string: HELLO WORLD
Capitalized string: HELLO WORLD
Capitalized string: HELLO WORLD
```

# Deep Dive
Now that we've seen a few different ways to capitalize strings in C, let's take a deeper look at how it all works. 

Method 1 and 2 both use built-in functions to capitalize the string. The `toupper()` function, found in the `<ctype.h>` library, converts a lowercase character to uppercase. Similarly, the `strupr()` function, found in the `<string.h>` library, converts an entire string to uppercase. 

Method 3 is a manual approach, where we check each character and either convert it using ASCII values or leave it as is. In case you're not familiar with ASCII values, each character has a corresponding number that C uses to represent it. For example, the ASCII value for lowercase 'a' is 97 and uppercase 'A' is 65. So by subtracting 32 from the lowercase ASCII value, we can convert it to uppercase.

By understanding these methods and how they work, you can choose the best approach for your specific situation. Keep in mind that different methods may have varying levels of efficiency and readability, so it's important to weigh your options.

# See Also
* [C Programming Tutorial](https://www.programiz.com/c-programming)
* [ASCII Table](https://www.ascii-code.com/)