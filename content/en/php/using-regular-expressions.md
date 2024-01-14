---
title:    "PHP recipe: Using regular expressions"
keywords: ["PHP"]
---

{{< edit_this_page >}}

##Why

Have you ever found yourself struggling to extract specific patterns of text from a string? Or maybe you needed to validate user input, such as phone numbers or email addresses? If so, regular expressions are the perfect solution for you. By using a combination of symbols and characters, regular expressions allow you to search for and manipulate text in a powerful and efficient way.

##How To

Regular expressions have a specific syntax that may seem intimidating at first, but fear not! Let's dive into some code examples to help you get started.

```PHP
// Check if a string contains the word "apple"
if (preg_match("/apple/", "I love apples")) {
    echo "Found the word apple!";
}
```
Output: Found the word apple!

```PHP
// Match a string with one or more digits at the beginning
if (preg_match("/^\d+/", "123abc")) {
    echo "Match found!";
}
```
Output: Match found!

```PHP
// Extract email addresses from a string
$string = "John's email is john@example.com and Jane's email is jane@example.com";
preg_match_all("/[\w\.]+@\w+\.\w+/",$string,$matches);
print_r($matches[0]);
```
Output: Array ( [0] => john@example.com [1] => jane@example.com )

##Deep Dive

Regular expressions offer a wide range of symbols and modifiers that allow for complex and precise searching and manipulation. Here are a few examples:

- `^` matches the beginning of a string
- `$` matches the end of a string
- `.` matches any single character
- `+` matches one or more occurrences
- `*` matches zero or more occurrences
- `?` matches zero or one occurrence
- `[]` defines a character set to match within
- `()` creates a subpattern to extract and capture specific parts of a string

It's important to note that regular expressions are case-sensitive by default, but this can be changed using modifiers such as `i` for case-insensitivity.

Regular expressions can also be used for advanced string replacement and manipulation. The functions `preg_replace()` and `preg_split()` allow you to replace and split strings using regular expressions.

##See Also

- [PHP Regular Expressions](https://www.php.net/manual/en/book.pcre.php)
- [Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Regex101](https://regex101.com/) - a web tool for testing and learning regular expressions.