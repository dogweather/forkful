---
title:                "Capitalizing a string"
html_title:           "Gleam recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
String capitalization is a common task when working with text data. It is used to format names, titles, and sentences in a visually appealing way. By capitalizing a string, we can make our code more readable and improve the user experience of our applications.

## How To
To capitalize a string in Gleam, we can use the `String.capitalize` function. This function takes in a string as its argument and returns that same string with the first letter capitalized. Let's see it in action:

```Gleam
let name = "jane"
let capitlized_name = String.capitalize(name)

io.println(capitlized_name)
// Output: Jane // 
```
In the above example, we have a variable called `name` with the value of "jane". We then use the `String.capitalize` function to capitalize the first letter, which is then stored in the `capitalized_name` variable. When we print out the `capitalized_name` variable, we can see that the value has been capitalized to "Jane". 

We can also use the `String.to_title_case` function to capitalize every word in a string. This can be useful when formatting titles or sentences. Let's take a look at an example:

```Gleam
let title = "the quick brown fox"
let title_case = String.to_title_case(title)

io.println(title_case)
// Output: The Quick Brown Fox // 
```
In this example, we have a variable called `title` with the string "the quick brown fox". We then use the `String.to_title_case` function to capitalize each word, resulting in "The Quick Brown Fox". This function is useful when working with titles or headings in our applications.

## Deep Dive
When using the `String.capitalize` function, it is worth noting that it will only capitalize the first letter of a string. This may not be desired if the string already contains capitalized letters. In this case, we can use the `String.to_title` function, which will capitalize the first letter and lowercase the rest of the letters in the string.

Additionally, if we want to manipulate strings further, we can also use the `String.split` function to split a string into a list of words based on a specific delimiter. This can be helpful when we want to capitalize each word individually before joining them back together using the `String.join` function.

## See Also
- Official Gleam Documentation on String module: https://gleam.run/documentation/stdlib/string.html
- Gleam String module source code: https://github.com/gleam-lang/gleam_stdlib/blob/master/gleam_stdlib/string/src/string.gleam