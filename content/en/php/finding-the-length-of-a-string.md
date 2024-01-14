---
title:    "PHP recipe: Finding the length of a string"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to find out the length of a string in your PHP code? Maybe you need to validate a user input or manipulate a string in some way. Knowing the length of a string is crucial in these situations, making it a useful skill to have in your PHP programming arsenal.

## How To
To find the length of a string in PHP, we can use the built-in function `strlen()`. This function takes in a string as an argument and returns the number of characters in that string. Let's take a look at an example:

```PHP
$input = "Hello World";
echo strlen($input);
```

The output of this code would be `11`, since there are 11 characters in the string "Hello World". Simple, right?

But what if we want to find the length of a string that contains special characters or is in a different language? In that case, we need to take into account the encoding of the string. Thankfully, the `mb_strlen()` function exists for this purpose. This function takes in a string and an optional parameter for the encoding, and it returns the length of the string, taking into account the specified encoding. Here's an example:

```PHP
$input = "こんにちは";
echo mb_strlen($input, 'UTF-8');
```

The output of this code would be `5`, as the `mb_strlen()` function correctly counts the number of characters in the Japanese greeting.

## Deep Dive
Now, for those of you who are curious, here's a deeper explanation of how `strlen()` and `mb_strlen()` work under the hood. In PHP, strings are represented as arrays of bytes, with each byte corresponding to a single character. The `strlen()` function simply counts the number of bytes in a string, which may not always reflect the actual number of characters, especially if the string contains multibyte characters. This is where the `mb_strlen()` function becomes useful, as it takes the encoding into consideration and correctly counts the number of characters in a string.

Additionally, if you're working with UTF-8 strings, you may come across the need to count the number of grapheme clusters (a combination of characters that form a single visual unit) in a string. In this case, the `grapheme_strlen()` function can be used.

## See Also
- [PHP Manual: strlen() function](https://www.php.net/manual/en/function.strlen.php)
- [PHP Manual: mb_strlen() function](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP Manual: grapheme_strlen() function](https://www.php.net/manual/en/function.grapheme-strlen.php)