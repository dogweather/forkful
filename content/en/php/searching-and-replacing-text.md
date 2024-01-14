---
title:    "PHP recipe: Searching and replacing text"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is an essential task for any programmer. Whether you're fixing typos, updating large datasets, or implementing new features, the ability to quickly and accurately find and replace specific words or phrases is crucial in the world of programming.

## How To 

To begin, let's take a look at the ```str_replace()``` function in PHP, which is used to search and replace text within a string. This function takes in three parameters: the search value, the replace value, and the string to search within.

```PHP
echo str_replace("Hello", "Hola", "Hello world!"); 
```

This code will replace all instances of "Hello" with "Hola" within the string "Hello world!", resulting in the output "Hola world!".

But what if we want to replace only the first instance of a word within a string? We can use the optional fourth parameter, ```$count```, to specify the maximum number of replacements.

```PHP
echo str_replace("Hey", "Hi", "Hey there Hey!", $count);
echo "\n";
echo $count;
```

This code will output "Hi there Hey!" and return a value of 1 for ```$count```. This can be useful when working with large datasets and wanting to limit the number of replacements made.

We can also use ```str_replace()``` to replace multiple words or phrases at once. Simply pass in arrays for the search and replace values.

```PHP
$search = array("Hello", "World");
$replace = array("Hola", "Mundo");
echo str_replace($search, $replace, "Hello World!");
```

This code will output "Hola Mundo!".

## Deep Dive

Now that we've covered the basics of ```str_replace()```, let's take a deeper dive into its functionality.

One important aspect to keep in mind is that ```str_replace()``` is case-sensitive. For example, if we have a string that contains the word "hello" and we use ```str_replace()``` to replace "Hello", it will not replace the lowercase "hello".

In addition, the search value and replace value can be either arrays or strings. This means we can use ```str_replace()``` to replace multiple words or phrases with multiple other words or phrases.

Another useful function to know is ```str_ireplace()```, which is case-insensitive. This means that it will replace words regardless of case, making it more versatile in some cases.

## See Also

For more information on searching and replacing text in PHP, check out the official PHP documentation on ```str_replace()``` and ```str_ireplace()```.

- [PHP: str_replace - Manual](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: str_ireplace - Manual](https://www.php.net/manual/en/function.str-ireplace.php)