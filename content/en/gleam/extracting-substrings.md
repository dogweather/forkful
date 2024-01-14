---
title:    "Gleam recipe: Extracting substrings"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

At some point in your coding journey, you may come across a situation where you need to extract a specific part of a string or text. This could be for various reasons such as data manipulation, formatting, or validation. The good news is, Gleam has a built-in function that makes this process quick and easy!

## How To

To extract substrings in Gleam, we will be using the `substring` function. This function takes in three parameters: the input string, the starting index, and the ending index. Let's take a look at a simple example:

```
let input = "Hello, world!"
let substring = substring(input, 7, 11)

IO.println(substring) // Output: "world"
```

In the above code, we have an input string of "Hello, world!" and we use the `substring` function to extract the part of the string that starts at index 7 (which is the letter "w") and ends at index 11 (which is the letter "d"). The output will be the substring "world".

You can also use variables for the starting and ending indexes, making the `substring` function even more useful. For example:

```
let input = "Hello, world!"
let start = 7
let end = 11
let substring = substring(input, start, end)

IO.println(substring) // Output: "world"
```

In the above code, we use variables for the starting and ending indexes, making it easier to manipulate the substring extraction based on different inputs.

It's important to note that the `substring` function is zero-indexed, meaning that the first character in the string has an index of 0. Keep this in mind when determining the starting and ending indexes for your substring.

## Deep Dive

The `substring` function in Gleam is actually a wrapper for the `slice` function in the standard library. The `slice` function takes in the same parameters as `substring`, but it returns a list of characters instead of a string.

This means that you can also use pattern matching and list manipulation techniques to work with your substrings. For example, you can match on the returned list and extract specific elements from the list, or you can use methods such as `list.to_string` to convert the list back into a string.

## See Also

- Official Gleam documentation on the `substring` function: https://gleam.run/documentation/stdlib/substring
- Official Gleam documentation on the `slice` function: https://gleam.run/documentation/stdlib/slice
- Learn more about pattern matching in Gleam: https://gleam.run/documentation/guides/pattern-matching