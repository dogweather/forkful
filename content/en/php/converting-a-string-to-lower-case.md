---
title:                "PHP recipe: Converting a string to lower case"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting a string to lower case may seem like a trivial task, but it can actually be quite useful in certain situations. For example, if you are working with user input or data from an external source, converting the string to lower case can help with uniformity and consistency in your code.

## How To
To convert a string to lower case in PHP, we can use the `strtolower()` function. Let's take a look at an example:

```PHP
$string = "Hello World";
echo strtolower($string);
```

The output of this code will be `hello world`. As you can see, all the letters in the string have been converted to lower case. This can also be useful when comparing strings, as it will ignore any differences in capitalization.

## Deep Dive
Now, let's take a deeper look at how the `strtolower()` function works. In PHP, strings are represented as an array of characters. The `strtolower()` function loops through each character in the string and checks if it is an uppercase letter. If it is, it converts it to its lowercase counterpart. This process continues until the end of the string is reached.

It is also important to note that the `strtolower()` function uses the current locale setting of the server to determine the lowercase versions of letters. This means that if you are working on a server with a different locale setting, the output of the function may vary.

## See Also
- [PHP strtolower() function documentation](https://www.php.net/manual/en/function.strtolower.php)
- [PHP strings documentation](https://www.php.net/manual/en/language.types.string.php)
- [PHP locales documentation](https://www.php.net/manual/en/function.setlocale.php)

That's all for converting strings to lower case in PHP! As you can see, it's a simple but useful function to have in your programming arsenal. Keep exploring and learning new things in PHP, and don't forget to check out the "See Also" links for more information. Happy coding!