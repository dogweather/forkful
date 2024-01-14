---
title:    "PHP recipe: Searching and replacing text"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a crucial part of any programming language, and PHP is no exception. Whether you are trying to fix typos or make large-scale changes to your code, having the ability to easily find and replace text can save you time and frustration. In this blog post, we will explore the various ways you can search and replace text in PHP.

## How To
Let's dive right into some coding examples to see how we can search and replace text in PHP. We will be using the `str_replace()` function, which takes three parameters: the text we want to replace, the replacement text, and the string we want to make the changes in.

```
<?php

// Simple example of replacing text
$text = "Hello, World!";
echo str_replace("World", "Universe", $text);
// Output: Hello, Universe!

// Case sensitive example
$text = "Hello, world!";
echo str_replace("World", "Universe", $text);
// Output: Hello, world! (no changes made)

// Replacing multiple instances of text
$text = "Hello, World, Hello!";
echo str_replace("Hello", "Hi", $text);
// Output: Hi, World, Hi!

// Replacing text in arrays
$names = ["John", "Jane", "Bob"];
$names = str_replace("John", "Jack", $names);
print_r($names);
// Output: Array ( [0] => Jack [1] => Jane [2] => Bob )
```

As you can see, the `str_replace()` function is versatile and can be used in various scenarios. It is also worth mentioning that there is a case-insensitive version of this function, `str_ireplace()`, which works the same way but ignores capitalization.

## Deep Dive
If you want more control over the replacement process, you can use the `preg_replace()` function, which utilizes regular expressions. This allows for more complex patterns to be searched and replaced. Let's take a look at an example:

```
<?php

// Using preg_replace() with regex
$text = "I love cats and dogs!";
echo preg_replace("/cats|dogs/", "good boys", $text);
// Output: I love good boys and good boys!

// Replacing only the first occurrence
$text = "Hello, Hello, Hello!";
echo preg_replace("/Hello/", "Hi", $text, 1);
// Output: Hi, Hello, Hello!
```

Regular expressions may seem intimidating at first, but once you get the hang of it, they can be incredibly useful for finding and replacing text in PHP.

## See Also
For more information on searching and replacing text in PHP, check out these resources:

- [PHP Manual on str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP Manual on preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions in PHP](https://www.w3schools.com/Php/php_regex.asp)

Now you have the tools to efficiently search and replace text in your PHP code. Happy coding!