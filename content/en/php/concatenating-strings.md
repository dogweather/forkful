---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Do you ever find yourself wanting to combine multiple pieces of text into one cohesive string? If so, then concatenating strings in PHP is the solution for you! By using this simple function, you can easily manipulate and organize text to fit your needs within your PHP code.

## How To

In PHP, concatenating strings is achieved using the `.` operator, which joins two strings together. Let's take a look at an example:

```PHP
$string1 = "Hello";
$string2 = "world!";
$result = $string1 . " " . $string2; // The result will be "Hello world!"
```

In the above example, we initialize two variables with strings and then use the `.` operator to combine them, including a space between them. This results in a new string, "Hello world!", being stored in the `$result` variable.

But what if we want to concatenate more than two strings together? No problem! You can continue using the `.` operator to add as many strings as you need. Let's see another example:

```PHP
$name = "John";
$greeting = "Hello";
$message = "Welcome to our website,";
$result = $greeting . " " . $name . "! " . $message; // The result will be "Hello John! Welcome to our website,"
```

As you can see, we can concatenate multiple strings by simply adding the `.` operator in between them. Just make sure to include any desired spaces or punctuation within the strings themselves.

## Deep Dive

While the `.` operator is sufficient for most string concatenation needs, PHP also offers the `.= ` operator for more efficiency. This operator not only concatenates two strings together but also assigns the result back to the first string, saving memory and improving performance.

```PHP
$name = "Alice";
$name .= " Smith"; // This is equivalent to $name = $name . " Smith";
echo $name; // The output will be "Alice Smith"
```

Additionally, we can use the `sprintf()` function to concatenate strings in a more precise and organized way. This function accepts a format string and a variable number of arguments, and then replaces any placeholders within the format string with the given arguments. Let's take a look at an example:

```PHP
$name = "Bob";
$age = 25;
$result = sprintf("My name is %s and I am %d years old.", $name, $age);
echo $result; // The output will be "My name is Bob and I am 25 years old."
```

In the above example, the `%s` acts as a placeholder for the first argument, `$name`, while the `%d` acts as a placeholder for the second argument, `$age`. This function allows for more control and flexibility when concatenating strings.

## See Also

To learn more about string concatenation and other PHP functions for manipulating strings, check out the following resources:

- [PHP String Concatenation Manual](https://www.php.net/manual/en/language.operators.string.php)
- [PHP String Functions Manual](https://www.php.net/manual/en/ref.strings.php)
- [PHP sprintf() Function Manual](https://www.php.net/manual/en/function.sprintf.php)