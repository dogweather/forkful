---
date: 2024-01-20 17:35:10.200352-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Here's the lowdown on joining strings together in PHP."
lastmod: '2024-03-13T22:44:49.416884-06:00'
model: gpt-4-1106-preview
summary: Here's the lowdown on joining strings together in PHP.
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
weight: 3
---

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
