---
title:                "Concatenating strings"
aliases:
- /en/php/concatenating-strings/
date:                  2024-01-20T17:35:16.268542-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is basically just stringing words together. Think of it like making a train out of words instead of carriages. Programmers do it to combine text, like names with greetings, or to build up messages and data that need to be flexible.

## How to:

In PHP, concatenating is all about the dot (`.'). Take two strings, put a dot between them, and voila! They're now one.

```PHP
$greeting = 'Hello, ';
$name = 'Alice!';
$message = $greeting . $name;
echo $message;
// Output: Hello, Alice!
```

Easy, right? Need to add and space? Just include it in a string and concatenate:

```PHP
$firstWord = 'Hello';
$space = ' ';
$secondWord = 'World!';
$sentence = $firstWord . $space . $secondWord;
echo $sentence;
// Output: Hello World!
```

And for the PHP pros, we can chain them together or use the shorthand (`.= `):

```PHP
$message = 'This';
$message .= ' is';
$message .= ' a';
$message .= ' sentence.';
echo $message;
// Output: This is a sentence.
```

## Deep Dive

Back in the olden days, PHP folks had to use the dot to smush strings together. It's like duct tape for words. Concatenation is essential because data isn't always delivered in the format we need it.

Regarding alternatives, there are a few. The `sprintf()` and `printf()` functions allow for formatted strings. Imagine you're creating a movie script with placeholders, and these functions fill in the actor's names.

```PHP
$format = 'There are %d monkeys in the %s';
echo sprintf($format, 5, 'tree');
// Output: There are 5 monkeys in the tree
```

But let's not forget our trusty friend, the `implode()` function. It's like a machine that takes an array of strings and a glue string and sticks them together.

```PHP
$array = ['Once', 'upon', 'a', 'time'];
echo implode(' ', $array);
// Output: Once upon a time
```

Another thing to consider is efficiency. For long strings or heavy operations, using `.` can be slower compared to other methods like `implode()` or even buffering the output. But for most everyday tasks, concatenation using the dot works like a charm.

## See Also

For the thirsty for more:

- The official PHP documentation on string operators is a great spot to know your tools: [PHP String Operators](https://www.php.net/manual/en/language.operators.string.php)
- To get a handle on more advanced string formatting, check out `sprintf()` and `printf()` functions: [PHP sprintf()](https://www.php.net/manual/en/function.sprintf.php)
- If you are looking to piece together elements of an array, read about `implode()`: [PHP implode()](https://www.php.net/manual/en/function.implode.php)
- For performance buffs, this conversation on concatenation vs. other methods is quite enlightening: [Stack Overflow: Efficient String Concatenation](https://stackoverflow.com/questions/3349753/efficient-string-concatenation-in-php)
