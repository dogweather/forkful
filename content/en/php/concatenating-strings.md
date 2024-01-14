---
title:    "PHP recipe: Concatenating strings"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Concatenating strings, or combining multiple strings into one, is a fundamental concept in PHP programming. It allows us to create dynamic and versatile outputs, making our code more efficient and readable.

## How To

To concatenate strings in PHP, we use the `.` operator. Let's look at an example:

```PHP
$name = "John";
$greeting = "Hello, ".$name;

echo $greeting;
```

In this example, we declared two variables - `$name` and `$greeting`. We then used the `.` operator to concatenate the string "Hello, " with the value stored in the `$name` variable. This results in the output: **Hello, John**.

We can also use the `.` operator to concatenate more than two strings:

```PHP
$first_name = "Jane";
$last_name = "Smith";
$full_name = $first_name." ".$last_name;

echo $full_name;
```

The output for this code would be **Jane Smith**, as we have combined the values stored in the `$first_name` and `$last_name` variables.

## Deep Dive

It's important to note that we can only concatenate strings in PHP using the `.` operator. If we try to use this operator on other data types, such as integers or booleans, PHP will automatically convert them into strings before concatenating.

In addition, we can use the shorthand `.= `operator to concatenate strings with an assigned variable. Let's look at an example:

```PHP
$message = "Welcome, ";
$name = "Emily";

$message .= $name;
// Equivalent to $message = $message.$name;

echo $message;
```

The output of this code would be **Welcome, Emily**. The `.= `operator concatenates the string "Welcome, " with the value of the `$name` variable and assigns it to the `$message` variable.

## See Also

- [PHP String Operators](https://www.php.net/manual/en/language.operators.string.php)
- [PHP String Manipulation](https://www.w3schools.com/php/php_string.asp)
- [PHP Concatenation Assignment Operator](https://www.php.net/manual/en/language.operators.assignment.php#language.operators.assignment.concat)