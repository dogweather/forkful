---
title:                "PHP recipe: Concatenating strings"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a commonly used technique in PHP programming that allows for the joining of multiple strings together. This can be useful for creating dynamic messages, building URLs, or formatting data to be displayed. Understanding how to concatenate strings can greatly enhance your coding skills and make your code more versatile.

## How To

Concatenating strings in PHP is fairly straightforward. The simplest way to do so is by using the concatenation operator, which is represented by a period `.`. Let's look at an example:

```
<?php
$name = "John";
$message = "Hello " . $name;

echo $message;
```

In this example, we have created two strings - `$name` and `$message`. To concatenate them together, we use the `.` operator, which joins the two strings together to form the final message, "Hello John".

You can also use the assignment operator, `.=`, to concatenate strings and assign the result to a variable. For example:

```
<?php
$message = "Hello ";
$message .= "John";

echo $message;
```

The result will be the same as the previous example. However, this method allows for more flexibility if you need to concatenate multiple strings together.

Another useful function for concatenating strings is `sprintf()`. This function allows you to format strings using placeholders, which will be replaced by the corresponding values. Let's see how it works:

```
<?php
$age = 30;
$message = sprintf("I am %d years old.", $age);

echo $message;
```

The result will be "I am 30 years old." The `%d` placeholder indicates we are replacing it with a decimal value, which in this case is the variable `$age`.

## Deep Dive

It's important to note that when concatenating strings, the order in which you join them together matters. Let's look at an example:

```
<?php
$name = "John";
$age = 30;
$message = "Hello, my name is " . $name . " and I am " . $age . " years old.";

echo $message;
```

In this example, we have concatenated the strings together in the desired order. However, if we were to switch the order of `$name` and `$age` in the concatenation, the output would be different - "Hello, my name is 30 and I am John years old." This is because PHP reads code from left to right, and will insert the values in the order they appear.

It's also worth mentioning that when concatenating strings, it's important to properly handle data types. For example, if you are concatenating a number with a string, the number will be automatically converted to a string. However, if you are concatenating a number with another number, they will be added together. To avoid unexpected results, it's best to explicitly convert data types when needed.

## See Also

- [PHP Manual: String Operators](https://www.php.net/manual/en/language.operators.string.php)
- [PHP Manual: sprintf()](https://www.php.net/manual/en/function.sprintf.php)
- [PHP Manual: Type Juggling](https://www.php.net/manual/en/language.types.type-juggling.php)