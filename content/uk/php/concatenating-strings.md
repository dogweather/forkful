---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:35:10.200352-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Concatenating strings means sticking them together end-to-end. Programmers do this to combine words and variables to create sentences or messages in their code.

## How to: (Як це зробити:)
Here's the lowdown on joining strings together in PHP:

```php
$greeting = "Привіт";
$name = "Василь";

// Using the dot operator to concatenate
$welcomeMessage = $greeting . ", " . $name . "!";
echo $welcomeMessage; // Outputs: Привіт, Василь!
```
Wrap your eyes around another way with double quotes:
```php
$food = "борщ";
$phrase = "Я люблю $food";
echo $phrase;  // Outputs: Я люблю борщ
```

## Deep Dive (Занурення у глибину)
Back in PHP’s younger days, string concatenation could only be done with the dot operator. Now you’ve also got the possibility of sticking variables right into double-quoted strings, a bit like how we used "борщ" in our example above.

There's always more than one way to skin a cat, or in this case, to stitch strings together. You could use `sprintf()` or `implode()` functions, but for basic concatenation, stick to the dot.

Some sharpen their performance by using braces around variables. Can make PHP parse a smidge faster.

```php
echo "Let's eat some {$food} together!";
```

## See Also (Дивись також)
Hungry for more? Take a look at these:
- PHP String Operators in the [PHP Manual](https://www.php.net/manual/en/language.operators.string.php)
- A deeper dive into strings in the [PHP String Functions List](https://www.php.net/manual/en/ref.strings.php)