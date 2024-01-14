---
title:                "Ruby recipe: Extracting substrings"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Ruby program? Maybe you want to grab a user's first name from their full name or extract a domain name from a URL. In these cases, using the `slice` or `[]` methods might be a bit tedious. That's where extracting substrings comes in, allowing you to easily and efficiently get the exact part of a string you need.

## How To

To extract substrings in Ruby, we use the `slice` method with the start and end indices of the desired substring. For example, if we have the string "Hello World" and want to extract "World", we can use the following code:

```Ruby
string = "Hello World"
substring = string.slice(6, 5)
puts substring #=> "World"
```

We can also use the `[]` method, which calls the `slice` method behind the scenes. The same example can be written as:

```Ruby
string = "Hello World"
substring = string[6, 5]
puts substring #=> "World"
```

Both methods take in the starting index and the number of characters to be extracted. Keep in mind that the index starts at 0, so the first character of a string is at index 0, the second at index 1, and so on. If we omit the second argument, the method will extract all characters from the start index to the end of the string.

We can also use negative indices to count from the end of the string. For example, `string.slice(-5, 5)` will extract the last 5 characters of the string. Additionally, we can use ranges to specify the start and end indices. The following code will extract the substring "o Wor" from our original string:

```Ruby
string = "Hello World"
substring = string[4..9]
puts substring #=> "o Wor"
```

## Deep Dive

Under the hood, the `slice` method creates a new string object and returns it, while the `[]` method modifies the original string object in place. This means that if we use `slice` on a frozen string, we will get an error, but it will work fine with `[]`. Additionally, both methods can take in regular expressions as arguments, allowing for more advanced substring extraction.

There is also another method in Ruby called `slice!` that works like `slice` but also removes the extracted substring from the original string. This can be useful if we want to manipulate the original string while extracting substrings from it.

## See Also

- [Ruby Documentation on `slice`](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [Ruby Documentation on `[]`](https://ruby-doc.org/core-2.7.1/String.html#method-i-5B-5D)
- [Ruby Documentation on `slice!`](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice-21)