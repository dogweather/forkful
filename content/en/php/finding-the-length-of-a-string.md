---
title:    "PHP recipe: Finding the length of a string"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

At some point in every web developer's journey, they will have to work with strings. And one of the most basic operations when dealing with strings is finding its length. It may seem like a simple task, but knowing how to properly find the length of a string can save you a lot of time and headaches in the future.

## How To

The built-in function in PHP for finding the length of a string is `strlen()`. Here's a simple example to show how it works:

```PHP
$string = "Hello World!";
echo strlen($string);
// Output: 12
```

As you can see, the function takes in a string as an argument and returns the length of the string in characters. But what if the string contains different character sets, such as emojis or special characters? In this case, `strlen()` may not return the correct length. That's where `mb_strlen()` comes in. This function takes into account the encoding of the string and gives the correct length. Let's see it in action:

```PHP
$string = "👋 Hello World!";
echo mb_strlen($string);
// Output: 14
```

Notice how the emojis counted as 2 characters, which is the correct length of this string.

But what if we want to find the length of a string that contains HTML tags? We can use `strip_tags()` to remove any HTML tags before finding the length. Here's an example:

```PHP
$string = "<h1>Hello World!</h1>";
echo strlen(strip_tags($string));
// Output: 12
```

## Deep Dive

Now, let's dive deeper into the `strlen()` function. This function actually calculates the number of bytes in a string, not the number of characters. This means it may not give the expected results for multibyte characters that occupy more than one byte. For example, the character "ä" in UTF-8 encoding takes up 2 bytes and will be counted as 2 characters by `strlen()`. To handle this, we can use `mb_strlen()` with the correct encoding parameter:

```PHP
$string = "Hühnerstall";
echo mb_strlen($string, "UTF-8");
// Output: 11
```

Another important thing to note is that `strlen()` counts the number of bytes, not the number of characters. This means that if the string contains special characters that take up more than 1 byte, the length may not be what you expect. For example, if the string contains the character "€", it will be counted as 3 characters by `strlen()`. It's essential to keep this in mind while working with strings and finding their length.

## See Also

- [PHP strlen() Function](https://www.php.net/manual/en/function.strlen.php)
- [PHP mb_strlen() Function](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP strip_tags() Function](https://www.php.net/manual/en/function.strip-tags.php)