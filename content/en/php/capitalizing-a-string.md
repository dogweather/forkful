---
title:    "PHP recipe: Capitalizing a string"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Why
Capitalizing strings is a common task in PHP programming that can be useful in various situations. It allows you to manipulate and format text in a specific way, such as making the first letter of a sentence uppercase or capitalizing proper nouns. In this blog post, we will explore how to capitalize strings in PHP and discuss the importance of this function.

##How To
To capitalize a string in PHP, we can use the built-in function `ucfirst()`. This function takes a string as an argument and returns a new string with the first character capitalized. Let's see an example:

```PHP
$string = "hello world";
$capitalized_string = ucfirst($string);
echo $capitalized_string;

//Output: Hello world
```

We can also use `ucwords()` function to capitalize the first letter of every word in a string.

```PHP
$string = "it's a beautiful day";
$capitalized_string = ucwords($string);
echo $capitalized_string;

//Output: It's A Beautiful Day
```

Both `ucfirst()` and `ucwords()` only capitalize the first letter, so if you want to make the whole string uppercase, you can use `strtoupper()` function.

```PHP
$string = "hello world";
$uppercase_string = strtoupper($string);
echo $uppercase_string;

//Output: HELLO WORLD
```

##Deep Dive
Now that we know how to capitalize strings in PHP, let's take a deeper dive into its significance. Capitalizing strings is not just about making text look more visually appealing, it also has practical uses in programming. For example, when working with user input, it is common to capitalize the first letter to ensure consistency and standardization in your database. This also applies to commonly used words and phrases, such as names, titles, and addresses.

Moreover, using `ucfirst()` or `ucwords()` functions in combination with string functions like `substr()` and `str_replace()` can give you more control over the formatting of your text. This can be especially helpful when dealing with large amounts of data.

##See Also
- [PHP Manual: ucfirst()](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP Manual: ucwords()](https://www.php.net/manual/en/function.ucwords.php)
- [PHP Manual: strtoupper()](https://www.php.net/manual/en/function.strtoupper.php)