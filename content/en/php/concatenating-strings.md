---
title:    "PHP recipe: Concatenating strings"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

One of the most fundamental tasks in programming is manipulating strings. Whether it's combining multiple strings together or extracting specific characters from a string, it's important to have a good understanding of string manipulation in order to write effective code. One particular aspect of string manipulation is concatenation, which involves combining two or more strings together to create a new string. In this blog post, we will explore the concept of concatenation in PHP and how it can be useful in various scenarios.

## How To

Concatenating strings in PHP is a simple and straightforward process. The concatenate operator in PHP is a period symbol (.) which is used to join two strings together. Let's take a look at some examples:

```PHP
$string1 = "Hello";
$string2 = "World";

// Concatenating two strings and storing the result in a new variable
$result = $string1 . $string2; 

// Output: HelloWorld
echo $result;

// Concatenating three strings and directly printing the result
echo $string1 . " " . $string2 . "!"; 

// Output: Hello World!
```

As you can see, the period operator allows us to easily combine strings of different lengths and add additional characters or space between them. This is particularly useful when constructing sentences or outputting dynamic data.

## Deep Dive

In PHP, concatenation can also be performed using the concatenation assignment operator (.=). This operator assigns the concatenated result back to the variable on the left side. Let's look at an example:

```PHP
$name = "John";
$name .= " Doe";

// Output: John Doe
echo $name;
```

In the above example, the string "Doe" is concatenated to the existing string stored in the variable $name, thus creating a new concatenated string "John Doe". This is especially useful when you need to keep appending strings to an existing one, without having to create a new variable each time.

## See Also

- [PHP String Operators](https://www.php.net/manual/en/language.operators.string.php)
- [PHP String Functions](https://www.php.net/manual/en/ref.strings.php)
- [PHP Concatenate vs Concatenate Assignment](https://www.php.net/manual/en/language.operators.assignment.php)