---
title:                "Gleam recipe: Extracting substrings"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a useful tool for manipulating strings in programming. It allows for the extraction of specific parts of a string, making it easier to work with and manipulate. In this blog post, we will explore how to use substring extraction in Gleam, a functional programming language designed for building scalable applications.

## How To

To extract substrings in Gleam, we will use the `String.slice` function. This function takes in three arguments: the string to be sliced, the starting index of the substring, and the ending index of the substring. Let's take a look at an example:

```Gleam
let string = "Hello, world!"

let substring = String.slice(string, 0, 5)

String.print(substring) // Outputs "Hello"
```

In this example, we declare a string variable and then use the `String.slice` function to extract the substring from the first index (0) to the fifth index, which includes the letter "o". The resulting substring is then printed using the `String.print` function.

We can also use negative indices in `String.slice` to start counting from the end of the string. For example, if we wanted to extract the last three characters of our string, we could use the following code:

```Gleam
let string = "Hello, world!"

let substring = String.slice(string, -3, String.length(string))

String.print(substring) // Outputs "ld!"
```

Notice how we use the `String.length` function to determine the ending index of the substring. This is because negative indices start counting from the end of the string, and we need to specify the total length of the string as the ending index.

## Deep Dive

It's important to note that the `String.slice` function in Gleam is inclusive on the starting index and exclusive on the ending index. This means that the character at the starting index is included in the resulting substring, but the character at the ending index is not. Let's look at another example to better understand this:

```Gleam
let string = "Hello, world!"

let substring = String.slice(string, 1, 5)

String.print(substring) // Outputs "ello"
```

In this example, the resulting substring starts from the first index (1) and ends at the fifth index, which includes the letter "o", but excludes the comma after it.

## See Also

To learn more about substring extraction in Gleam, check out the official documentation and the Gleam community forum.

- [Official Documentation](https://gleam.run/documentation)
- [Gleam Forum](https://gleam.run/forum)