---
title:    "PHP recipe: Using regular expressions"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are a powerful tool for string manipulation in PHP. They allow for efficient searching and replacing of patterns within strings, making complex tasks much simpler to accomplish. Utilizing regular expressions can greatly improve the functionality and readability of your PHP code.

## How To

To use regular expressions in PHP, we first need to use the `preg_match()` function to check if a pattern exists within a string. Let's take a look at an example:

```PHP
$string = "Hello World!";
$pattern = "/World/";

if (preg_match($pattern, $string)) {
  echo "Pattern found!";
} else {
  echo "Pattern not found!";
}
```

In this example, we have a string containing the phrase "Hello World!" and we are searching for the pattern "/World/" within it. The `preg_match()` function takes in two parameters, the pattern to search for and the string to search within. In this case, the pattern is found within the string, so "Pattern found!" will be echoed.

We can also use regex to replace patterns within a string using the `preg_replace()` function. Let's see how this looks in code:

```PHP
$string = "Hello John!";
$pattern = "/John/";
$replacement = "Jane";

$new_string = preg_replace($pattern, $replacement, $string);
echo $new_string;
```

In this example, we have a string containing the name "John" and we want to replace it with "Jane". By using the `preg_replace()` function, we can easily accomplish this. The output will be "Hello Jane!".

## Deep Dive

Regular expressions in PHP follow a specific syntax that may seem intimidating at first, but it becomes easier to understand with practice. Some common characters used in regex include:

- `.` - Matches any character except for new line.
- `*` - Matches the previous character 0 or more times.
- `+` - Matches the previous character 1 or more times.
- `?` - Matches the previous character 0 or 1 time.
- `|` - Matches either the expression before or after the pipe.
- `[]` - Matches any character within the brackets.
- `()` - Groups together expressions and remembers the matched values.

To learn more about regular expressions in PHP, you can refer to the PHP documentation or online tutorials. It may also be helpful to practice using regex with online tools like Regex101 or Debuggex.

## See Also

- [PHP Regular Expressions Documentation](https://www.php.net/manual/en/book.pcre.php)
- [Regex101](https://regex101.com/)
- [Debuggex](https://www.debuggex.com/)