---
title:                "Gleam recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why 

Finding the length of a string might seem like a simple task, but it is a fundamental skill that every programmer should have in their toolkit. Knowing the length of a string allows you to manipulate and analyze it in various ways, making it a crucial step in many programming tasks. In this blog post, we will dive into how to find the length of a string in Gleam.

## How To

To find the length of a string in Gleam, we will use the ```length``` function from the standard library's ```string``` module. Let's take a look at an example:

```Gleam
import string

let name = "John"

let length = string.length(name)

# Output: 4
```

In the above code, we import the ```string``` module and create a variable ```name``` with the value "John". Then, we use the ```length``` function to find the length of the string and store it in a variable called ```length```. Finally, we print the value of ```length```, which will be 4 in this case.

But what if we want to find the length of a string with non-ASCII characters? Gleam has you covered with its Unicode support. Let's see an example:

```Gleam
import string

let name = "李华"

let length = string.length(name)

# Output: 2
```

Just like before, we import the ```string``` module and assign a string with non-ASCII characters to the variable ```name```. When we use the ```length``` function, it correctly returns the length of the string, which is 2. This shows the power of Gleam's Unicode support, making it easier to work with non-ASCII characters.

## Deep Dive

Under the hood, the ```length``` function simply iterates over each character in the given string and counts them. It is a relatively simple but essential operation that can be quite powerful when combined with other string manipulation functions.

One thing to keep in mind when finding the length of a string is that it will also count any spaces or punctuation marks present in the string. So, if you want to exclude them, you can use the ```trim``` function from the ```string``` module before finding the length.

## See Also

- ```length``` function documentation: https://gleam.run/modules/string.html#length
- Unicode support in Gleam: https://gleam.run/tour/unicode.html
- Other useful string manipulation functions in the ```string``` module: https://gleam.run/modules/string.html