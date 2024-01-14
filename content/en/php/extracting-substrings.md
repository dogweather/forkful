---
title:    "PHP recipe: Extracting substrings"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Have you ever come across a situation where you need to extract a specific part of a larger string? Maybe you want to extract a username from an email address, or retrieve the last name from a full name. This is where extracting substrings come in handy.

## How To
Extracting substrings in PHP is a simple process that involves using built-in string functions. Let's take a look at some examples:

```
// Extracting a username from an email address
$email = "john.doe@example.com";
$username = strstr($email, '@', true);
echo $username; // Output: john.doe

// Extracting a last name from a full name
$name = "Jane Smith";
$lastname = substr($name, strpos($name, " ") + 1);
echo $lastname; // Output: Smith
```

In the first example, we used the `strstr()` function to find the position of the '@' symbol and then passed in a third parameter of `true` to return the part of the string before the '@' symbol. In the second example, we used the `substr()` function to extract the part of the string after the space.

You can also use the `explode()` function to split a string into an array, using a specific delimiter.

```
// Extracting a street name from a full address
$address = "123 Main St., City, State";
$street = explode(",", $address)[0];
echo $street; // Output: 123 Main St.
```

## Deep Dive
There are a few things to keep in mind when extracting substrings in PHP. Firstly, PHP string functions are case-sensitive, so make sure you are using the right case when searching for a specific substring. Secondly, if a substring is not found, the functions will return `false`, so it's important to handle this scenario in your code. Lastly, substrings in PHP are zero-indexed, meaning the first character of a string is at position 0, the second character is at position 1, and so on.

There are many other string functions in PHP that can be used for extracting substrings, such as `str_replace()`, `preg_match()`, and `strtok()`. Experiment with different functions to find the one that works best for your specific situation.

## See Also
- [PHP: String Functions](https://www.php.net/manual/en/ref.strings.php)
- [W3Schools: PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)
- [PHP String Functions Cheat Sheet](https://www.unixtimestamp.com/php-cheat-sheet/php-string-functions.php)

With the power of PHP string functions, you can easily extract specific parts of a string without having to manually manipulate it. This can save you time and effort in your programming tasks. Give it a try in your next project and see how it can benefit you. Happy coding!