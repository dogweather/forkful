---
title:                "Using regular expressions"
html_title:           "PHP recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are a powerful and versatile tool for text processing. They allow us to search, match, and manipulate patterns of text, making it easier and more efficient to work with strings of data. Whether you're a web developer, data scientist, or just need to extract information from a text file, regular expressions can greatly enhance your coding capabilities.

## How To
To use regular expressions in PHP, we first need to create a pattern using a combination of symbols and characters. Here's an example of a simple regex pattern using the `preg_match()` function:

```PHP
$string = "Hello, world!";
$pattern = "/^[A-Za-z, ]+$/";

if(preg_match($pattern, $string)){
  echo "Pattern matched!";
} else {
  echo "No match found.";
}

// Output: Pattern matched!
```

In this example, we're checking if the string only contains letters, commas, and spaces. Let's break down the pattern:
- `/` - Starting delimiter
- `^` - Anchor at the beginning of the string
- `[A-Za-z, ]+` - Character group that includes letters, comma, and space; the `+` means that this group should occur one or more times
- `$` - Anchor at the end of the string
- `/` - Closing delimiter

We can also use regex to extract data from a string. Take this example:

```PHP
$string = "Email: example@example.com";
$pattern = "/\b[A-Za-z]+@[A-Za-z]+\.[A-Za-z]+/";

preg_match($pattern, $string, $matches);
print_r($matches);

// Output: Array ( [0] => example@example.com )
```

In this case, we're extracting an email address from the string by using the `\b` word boundary and `\.` to match the dot in the email domain.

## Deep Dive
Regular expressions can be complex and intimidating, but they are a valuable skill to have as a programmer. Here are some tips to help you dive deeper into regex:

- Use online regex testers to experiment and fine-tune your patterns.
- Familiarize yourself with the different symbols and characters used in regex, such as anchors, character classes, and quantifiers.
- Given a task, break it down into smaller parts and create patterns for each step. Then, combine them to create a complete regex solution.
- Utilize the power of capturing groups to easily extract specific parts of a string.
- Take advantage of the various flags available, such as `i` for case-insensitive matching and `g` for global search.

Remember, practice makes perfect. The more you use regex, the more comfortable and confident you'll become in using it.

## See Also
- [PHP Regular Expressions Documentation](https://www.php.net/manual/en/book.pcre.php)
- [Regex101 - Online Regex Tester](https://regex101.com)
- [The Regex Coach - Interactive Online Tutorial](https://www.weitz.de/regex-coach)