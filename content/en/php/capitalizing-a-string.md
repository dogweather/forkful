---
title:                "PHP recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Why

When working with strings in a PHP application, you may come across situations where you need to capitalize a string. Whether it's for aesthetic purposes or to match specific formatting requirements, capitalizing strings can be a useful tool in your programming arsenal.

##How To

To capitalize a string in PHP, you can use the built-in `ucfirst()` function. This function takes in a string as its argument and returns the same string with the first character capitalized. Let's take a look at an example:

```
<?php

// Define a string
$str = "hello world";

// Capitalize the string
$capStr = ucfirst($str);

// Output: Hello world
echo $capStr;

?>
```

As you can see, the `ucfirst()` function takes in our string "hello world" and returns "Hello world" with the first letter capitalized.

But what if you want to capitalize every word in a string? In that case, you can use the `ucwords()` function. This function takes in a string and returns the same string with the first letter of each word capitalized. Let's see it in action:

```
<?php

// Define a string
$str = "i love programming";

// Capitalize every word in the string
$capStr = ucwords($str);

// Output: I Love Programming
echo $capStr;

?>
```

##Deep Dive

Now, let's take a closer look at how the `ucfirst()` and `ucwords()` functions work. In PHP, strings are considered arrays of characters, with each character having its own index. When using `ucfirst()`, the function simply converts the character at index 0 to uppercase, leaving the rest of the string unchanged.

Similarly, `ucwords()` converts the first character of every word to uppercase by finding any space or punctuation marks and using the next character as the starting point for capitalization.

It's also worth noting that both `ucfirst()` and `ucwords()` only convert characters to uppercase in accordance with the ASCII table. This means that any accented or non-English characters may not be capitalized properly.

##See Also

For more information on string manipulation in PHP, check out the following resources:

- [PHP Strings Documentation](https://www.php.net/manual/en/language.types.string.php)
- [String Functions in PHP](https://www.w3schools.com/php/php_string.asp)