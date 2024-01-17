---
title:                "Converting a string to lower case"
html_title:           "Go recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case means changing all the letters in a string to their lowercase counterparts. It's a common operation in programming since it helps with comparison and string manipulation tasks. 

## How to:

To convert a string to lower case in Go, we can use the built-in ```strings.ToLower()``` function. Let's see it in action:

```
//declare a string variable
str := "Hello World!"

//convert string to lowercase
lowerStr := strings.ToLower(str)

//print the result
fmt.Println(lowerStr)
```

The output would be: ```hello world!```


## Deep Dive:

### Historical Context:
Manipulating strings has always been an essential part of programming, and converting a string to lowercase is a common task in many programming languages. In the past, programmers had to use complex algorithms or create custom functions to achieve this task. However, with the rise of modern programming languages like Go, this process has become much more streamlined and efficient, thanks to built-in functions like ```strings.ToLower()```.

### Alternatives:
While Go has a built-in function for converting strings to lowercase, other programming languages may have different approaches. For example, in Python, we can use the ```lower()``` method, while in Java, we would use the ```toLowerCase()``` method. As a programmer, it's essential to be familiar with the different syntax and functions of each language.

### Implementation details:
Internally, the ```strings.ToLower()``` function in Go uses the ```unicode``` package to map the unicode table to lowercase. It also handles special cases like Turkish letters "İ" and "I" to ensure accurate results. 

## See Also:

- [Go documentation](https://golang.org/pkg/strings/#ToLower)
- [Unicode Table](https://unicode-table.com/en/)
- [Java documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
- [Python documentation](https://docs.python.org/3/library/stdtypes.html#str.lower)