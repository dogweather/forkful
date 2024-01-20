---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation is a way to embed variables within a string. Why bother? It makes your PHP code neater and more readable by avoiding excessive concatenation.

## How to: 
Let's get straight to coding. In PHP, you can do string interpolation using double quotes or the heredoc syntax. Here's a simple example to show you both:

```PHP
//Using double quotes
$name = "Fizzy";
echo "Hello, $name! How are you?";

//Using Heredoc syntax
$message = <<<MSG
Hello, $name! How have you been?
MSG;
echo $message;
```

Run these, and both will output:

```
Hello, Fizzy! How are you?
Hello, Fizzy! How have you been?
```

Check how we inserted `$name` directly into the strings. No concatenation. Easy-peasy!

## Deep Dive
Python and Perl inspired PHP's string interpolation feature. It showed up in PHP 4 (2000) and is better than concatenation because it’s more efficient at runtime.

Your alternatives are string concatenation or using template engines like Twig or Smarty. For simple needs, stick to interpolation or concatenation. Else, consider a template engine.

Remember: only double-quoted strings and heredoc strings can interpolate variables. Single-quoted strings? Nope, they'll read your variables as plain text.

## See Also
1. PHP Manual on [String Interpolation](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)
2. PHP Manual on [Heredoc Syntax](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc)
3. [Twig](https://twig.symfony.com/), [Smarty](https://www.smarty.net/)
   
You're good to go! Happy coding with PHP.