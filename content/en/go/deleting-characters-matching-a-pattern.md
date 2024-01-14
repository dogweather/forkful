---
title:    "Go recipe: Deleting characters matching a pattern"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself working with a string or text data and needing to delete certain characters that match a specific pattern? Maybe you're working on a program that needs to clean up user inputs or parse through large amounts of data. Whatever the reason may be, knowing how to efficiently delete characters matching a pattern can be a valuable skill to have when working with string manipulation in Go.

## How To

To demonstrate how to delete characters matching a pattern in Go, let's create a simple program that takes in a string and removes all digits from it. Below is the code for our program:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // User input string
    input := "H3llo W0rld!"

    // Compile regex pattern to match digits
    regex := regexp.MustCompile("\\d")

    // Use ReplaceAllString function to replace digits with empty string
    output := regex.ReplaceAllString(input, "")

    // Print output
    fmt.Println("Without digits:", output)
}
```

The first thing we do is import the necessary packages for our program, which in this case are "fmt" for printing and "regexp" for regular expressions. Then, we declare our main function and create a variable for our user input string. After that, we use the "regexp.MustCompile" function to compile our pattern, in this case "\\d" which matches any digit. Finally, we use the "ReplaceAllString" function to replace all digits in the input string with an empty string and print the output.

Running this program will result in the following output:

```
Without digits: Hllo Wrld!
```

As you can see, all the digits have been successfully removed from the string. This is just one example of how you can delete characters matching a pattern in Go. With regular expressions, the possibilities are endless and you can create more complex patterns to fit your specific needs.

## Deep Dive

Knowing how to delete characters matching a pattern is useful not just in string manipulation, but also in data processing and validation. Regular expressions have a powerful syntax that allows for a wide range of pattern matching capabilities. It may seem intimidating at first, but with practice, you can become proficient in using them for various tasks.

In our example, we used the "\\d" pattern to match digits, but there are many other special characters and modifiers that can be used to create more complex patterns. Here are a few examples:

- "\\w" matches any word character (letters, digits, and underscore)
- "\\s" matches any whitespace character (space, tab, line break)
- "[]" allows you to specify a range of characters to match, e.g. "[a-z]" matches any lowercase letter

You can also use modifiers like the asterisk "*" to match zero or more occurrences of the previous character and the plus sign "+" to match one or more occurrences. There are many more patterns and modifiers that you can explore in the Go regular expressions documentation.

## See Also

To learn more about regular expressions in Go and how to use them for pattern matching, check out these resources:

- [The Go Blog: Regular Expressions in Go](https://blog.golang.org/regular-expressions)
- [Regular Expressions in Go Package Documentation](https://golang.org/pkg/regexp/)
- [Learn How To Use Regular Expressions in Go Tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go)