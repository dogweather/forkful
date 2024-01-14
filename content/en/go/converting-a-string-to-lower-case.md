---
title:    "Go recipe: Converting a string to lower case"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in many programming languages, including Go. This is useful for making strings case-insensitive when performing comparisons or for cleaning up user input.

## How To

To convert a string to lower case in Go, we can use the `strings.ToLower()` function. Let's look at an example:

```Go
input := "Hello World"
lowerCase := strings.ToLower(input)
fmt.Println(lowerCase)
```

The output of this code will be `hello world`. We can see that the original string has been converted to all lower case.

We can also use this function to convert individual characters to lower case. Let's see how this works:

```Go
input := 'G'
lowerCase := strings.ToLower(string(input))
fmt.Println(lowerCase)
```

The output of this code will be `g`. We first convert the character `G` to a string using `string(input)` and then use `strings.ToLower()` to convert it to lower case.

## Deep Dive

Under the hood, the `strings.ToLower()` function uses the Unicode mapping for case conversion. This means that it will handle different alphabets and special characters correctly. It also takes into account any language-specific rules for case conversion.

One important thing to note is that the `strings.ToLower()` function returns a new string instead of modifying the original string. This is because strings in Go are immutable, meaning they cannot be changed. Therefore, we must assign the result to a new variable as shown in the examples above.

## See Also

- [The strings package documentation](https://golang.org/pkg/strings/)
- [Unicode case mappings](https://unicode.org/copyright.html)