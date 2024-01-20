---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Searching and Replacing Text in PHP

## What & Why?
Text searching and replacing is a common task in programming where specific strings are located, and possibly modified, within a chunk of text. Programmers do this to manipulate data, automate edits and extract meaningful details.

## How to:
There are few latest methods in PHP to search and replace text. 

### str_replace Function
The str_replace function is probably the first method to consider. Below is a simple example:

```PHP
$text = "I love Python";
$newText = str_replace("Python", "PHP", $text);
echo $newText;
```
The function's output will be: `"I love PHP"`.

### preg_replace Function
If you need more advanced features, for instance, to support regular expressions, you can use preg_replace. Here's how:

```PHP
$text = "I love Python";
$newText = preg_replace("/Python/", "PHP", $text);
echo $newText;
```
This will also output: `"I love PHP"`.


## Deep Dive
Text searching and replacing have roots that go back to the first programs. As PHP evolved, additional functions like str_replace and preg_replace with more versatile options have been introduced.

There are other alternatives that you can use according to your preference and the complexity of the task. For instance, `strtr` function if you're looking for a reliable performance with larger text.

In terms of implementation, both str_replace and preg_replace do a simple task. They look for a specific pattern in the string (text), and replace it with new pattern (replacement). The performance of these functions, however, relies heavily on the size and complexity of the text and replacement patterns.

## See Also:
Below are some useful related PHP manuals and resources:

[str_replace](https://www.php.net/manual/en/function.str-replace.php) 
[preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
[String manipulation](https://www.w3schools.com/php/php_ref_string.asp) in PHP tutorial
[PHP Regular Expressions](https://www.w3schools.com/php/php_regex.asp) at w3schools.