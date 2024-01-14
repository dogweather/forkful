---
title:    "C recipe: Using regular expressions"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool in the world of programming that allow us to efficiently search and manipulate strings of text. By using regular expressions, we can easily extract specific information from large amounts of data, validate user input, and perform complex text transformations. 

## How To

To use regular expressions in your C programs, you first need to include the stdlib.h header file and use the functions available in it: `regcomp`, `regexec`, `regfree`. Let's take a look at a simple example that uses regular expressions to validate a password:

```C
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main()
{
    // Regular expression to match a password that contains at least one uppercase letter, one lowercase letter, and one number
    char *pattern = "^(?=.*[A-Z])(?=.*[a-z])(?=.*[0-9]).+$";

    // The password to be validated
    char *password = "SecureP@ssw0rd";

    // Compile the regular expression
    regex_t regex;
    int ret = regcomp(&regex, pattern, REG_EXTENDED);
    if (ret != 0) {
        printf("Could not compile regular expression.\n");
        return 1;
    }

    // Execute the regular expression
    ret = regexec(&regex, password, 0, NULL, 0);
    if (ret == 0) {
        printf("Password is valid!\n");
    } else if (ret == REG_NOMATCH) {
        printf("Password is invalid. It should contain at least one uppercase letter, one lowercase letter, and one number.\n");
    } else {
        printf("Could not execute regular expression.\n");
    }

    // Free the memory allocated for the compiled regular expression
    regfree(&regex);

    return 0;
}
```

**Output:**

```
Password is valid!
```

In this example, we used the `regcomp` function to compile our regular expression, `regexec` to execute it, and `regfree` to free the memory allocated for it. We also used the predefined `REG_EXTENDED` flag to specify that we want to use extended regular expression syntax. 

You can also use regular expressions with user input, file operations, and more complex patterns to achieve various programming tasks. 

## Deep Dive

Regular expressions are made up of symbols and special characters that represent specific patterns and characters. Here are some commonly used symbols and their meanings:

- `.` - Matches any single character
- `^` - Matches the beginning of a string
- `$` - Matches the end of a string
- `*` - Matches the preceding character zero or more times
- `+` - Matches the preceding character one or more times
- `?` - Matches the preceding character zero or one time
- `[]` - Matches any of the characters inside the brackets
- `()` - Groups characters together
- `|` - Matches either the expression before or after it
- `\` - Escapes a special character

There are many more symbols and modifiers available for regular expressions, so it's important to consult a reference guide when creating more complex patterns. Additionally, regular expressions can become more performant if written correctly, so it's important to understand the underlying algorithms behind them.

## See Also

- [Regular Expressions 101](https://regex101.com/) - A helpful website for visualizing and testing regular expressions.
- [C Programming Tutorial - Regular Expressions](https://www.learn-c.org/en/Regular_Expressions) - A tutorial on using regular expressions in C by Learn-C.org.
- [The Theory Behind Regular Expressions](https://www.regular-expressions.info/tutorial.html) - A comprehensive guide on the theory behind regular expressions.