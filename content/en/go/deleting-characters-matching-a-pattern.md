---
title:    "Go recipe: Deleting characters matching a pattern"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern in Go can be a useful programming task for a number of reasons. It can help with data cleaning, removing sensitive information, or manipulating strings in specific ways. By understanding how to delete characters matching a pattern in Go, you can enhance the functionality of your code and make it more robust.

## How To

To delete characters matching a pattern in Go, we can use the `ReplaceAllString()` function from the `regexp` package. This function takes in three parameters - the pattern to be matched, the replacement string, and the input string. It returns a new string with the matched pattern replaced by the specified replacement string.

For example, let's say we have a string `s` that contains the following sentence:

```Go
s := "I love Go programming!"
```

If we want to delete all vowels from this string, we can use the following code:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    s := "I love Go programming!"
    re := regexp.MustCompile("[aeiouAEIOU]")
    newStr := re.ReplaceAllString(s, "")
    fmt.Println(newStr)
}
```

The output of this program would be:

```
lv G prgrmmng!
```

In the above code, we first declare a regular expression `re` that matches all vowels, both lowercase and uppercase. Then, we use the `ReplaceAllString()` function to replace all occurrences of vowels with an empty string. Finally, we print out the new string without vowels.

## Deep Dive

Regular expressions, or regex, are a powerful tool for pattern matching in strings. In Go, the `regexp` package provides functions and structs for working with regular expressions. The `ReplaceAllString()` function is just one example of using regular expressions for manipulating strings.

To make our pattern matching more specific, we can use special characters in our regular expression. For example, if we only want to delete vowels that are followed by a space, we can use the `(*SKIP)` special feature in our regex. This tells the compiler to skip over the matched pattern instead of returning it. Let's see this in action:

```Go
...
re := regexp.MustCompile("(?i)[aeiou] (*SKIP) ")
...
```

The `(?i)` indicates that we want to match the pattern case insensitively. After that, we specify the pattern to be matched, followed by `(*SKIP)`. This will ensure that any vowels followed by a space are skipped and not included in the final output.

There are many other special characters and features that can be used in regular expressions, making them a powerful tool for string manipulation in Go.

## See Also

- [Official Go Regexp Package Documentation](https://pkg.go.dev/regexp)
- [Regular Expressions Cheat Sheet for Go](https://devhints.io/regexp)
- [An Introduction to Regular Expressions in Go](https://medium.com/@avinashgupta75/introduction-to-regular-expression-in-go-b5e720796e3)

By mastering the art of deleting characters matching a pattern in Go, you can elevate your coding skills and make your programs more versatile. So go ahead and give it a try in your next Go project!