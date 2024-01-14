---
title:                "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Concatenating strings is a common practice in PHP programming, especially when it comes to dynamic content. It allows developers to combine multiple strings into one, making it easier to manipulate and display data in a desired format.

## How To
Concatenating strings in PHP is done using the `.` operator. Let's say we have two strings, `$firstName` and `$lastName`, and we want to combine them to form a full name. We can do that using the following code:

```PHP
$firstName = "John";
$lastName = "Doe";
$fullName = $firstName . " " . $lastName;

echo $fullName;
```
This will output `John Doe` on the screen.

We can also use concatenation to add additional text or variables within a string. For example:

```PHP
$message = "Hello, my name is " . $fullName;
echo $message;
```
This will output `Hello, my name is John Doe`.

It is important to note that when using the `.` operator, the spaces or punctuation between the strings must be included within the quotation marks.

## Deep Dive
In addition to using the `.` operator, PHP also provides the `.= ` operator for concatenation. This allows us to concatenate multiple strings in a single line of code. For example:

```PHP
$message = "Hello, ";
$message .= "my name is ";
$message .= $fullName;

echo $message;
```
This will still output `Hello, my name is John Doe`.

We can also use concatenated strings in functions and loops to dynamically generate data. This is particularly useful when working with databases and displaying information on a website.

## See Also
- [PHP String Operators](https://www.php.net/manual/en/language.operators.string.php)
- [PHP Concatenation Assignment Operators](https://www.php.net/manual/en/language.operators.assignment.php#example-398)

Now that you know how to concatenate strings in PHP, you can apply this knowledge to your own projects and enhance the functionality of your code. Happy coding!