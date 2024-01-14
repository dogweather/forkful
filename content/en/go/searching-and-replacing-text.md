---
title:    "Go recipe: Searching and replacing text"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Have you ever found yourself manually replacing text in a large codebase? It can be tedious and time-consuming, not to mention prone to human error. Luckily, Go has a built-in function that can make this task much easier - the "strings" package.

## How To
To use the "strings" package for searching and replacing text, we first need to import it into our program. In the below example, "?" represents the package path of your program.

```
import "strings"
```

Next, we can use the "ReplaceAll" function from the "strings" package to search for a specific substring and replace it with a new value.

```
str := "This is a sample string"
newStr := strings.ReplaceAll(str, "sample", "replacement")
```

The above code will search for the substring "sample" within the string variable and replace it with the word "replacement". The output of "newStr" will be "This is a replacement string".

We can also specify how many replacements we want to make by using the optional "n" parameter.

```
str := "This is a sample string"
newStr := strings.ReplaceAll(str, "s", "S", 2)
```

In this code, the "s" in "sample" will be replaced with "S" but only for the first two occurrences. The output of "newStr" will be "ThiS iS a sample string".

## Deep Dive
The "ReplaceAll" function is just one of many functions within the "strings" package that can be used for searching and replacing text. Other useful functions include "Replace", "ReplaceAllString", and "ReplaceAllLiteral". 

It's important to note that the "ReplaceAll" function is case-sensitive, meaning if you use "ABC" as the substring to search for, it will not replace "abc" or "Abc". However, if you want a case-insensitive search, you can use "ReplaceAllIgnoreCase" function instead.

## See Also
- [Go strings package documentation](https://golang.org/pkg/strings/)
- [Go by Example: String Functions](https://gobyexample.com/string-functions)
- [Golang GFG - String Replace Function](https://www.geeksforgeeks.org/golang-gfg-string-replace-function/)