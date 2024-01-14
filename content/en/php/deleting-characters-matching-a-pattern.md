---
title:    "PHP recipe: Deleting characters matching a pattern"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to remove certain characters from a string in your PHP code? Whether it's unwanted whitespace, special characters, or specific patterns, it can be quite a hassle to manually delete them one by one. This is where the power of PHP programming comes in handy, as you can easily automate this task with just a few lines of code. In this blog post, we'll explore the "str_replace" function in PHP and how it can help you efficiently delete characters matching a given pattern.

## How To

To begin, we'll start by setting up a simple string with some unwanted characters:

```
$myString = "Hello, *world*!";
```

In this example, we want to remove the asterisks surrounding the word "world". To do this, we can use the "str_replace" function in the following way:

```
$newString = str_replace("*", "", $myString);
```

By passing in the character we want to replace (in this case, the asterisk) and replacing it with an empty string, we effectively remove it from our original string. Let's take a look at the output of our new string:

```
Hello, world!
```

Success! The asterisks have been deleted from our string. But what if we want to remove multiple characters or patterns at once? We can do that too with the "str_replace" function. Let's take a look at another example:

```
$myString = "Welcome to ***my*** blog!";
```

In this case, we want to remove all instances of three asterisks from our string. We can achieve this by passing in an array of characters to our "str_replace" function, like so:

```
$newString = str_replace(["*", "*", "*"], "", $myString);
```

This will replace all occurrences of three asterisks with an empty string, effectively deleting them from our original string. Let's check the output:

```
Welcome to my blog!
```

As you can see, all the asterisks have been removed, leaving us with a clean string. The "str_replace" function also has the ability to replace multiple characters with a single character, giving you even more flexibility in your string manipulation.

## Deep Dive

Now that we've seen how to use the "str_replace" function in practical scenarios, let's take a deeper look into how it works. This function takes in three parameters: the string or array of strings to be replaced, the string or array of strings to replace them with, and the string to be modified. It then goes through the string and replaces each instance of the given characters or patterns with the replacement string. This process continues until all occurrences have been replaced.

It's important to note that the "str_replace" function is case-sensitive, meaning that it will only replace characters or patterns that match exactly. However, you can use the "str_ireplace" function if you want to perform a case-insensitive replacement.

## See Also

- [PHP str_replace documentation](https://www.php.net/manual/en/function.str-replace.php)
- [PHP str_ireplace documentation](https://www.php.net/manual/en/function.str-ireplace.php)
- [PHP string functions](https://www.w3schools.com/php/php_ref_string.asp)

And there you have it – a quick overview of how to delete characters matching a pattern in PHP using the "str_replace" function. With its simple syntax and powerful functionality, this function can save you time and effort when working with strings in your PHP code. Give it a try in your next project and see the magic for yourself!