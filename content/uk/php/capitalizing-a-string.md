---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?
Capitalizing a string means shifting all letters to uppercase. Programmers capitalize for emphasis, titles, or consistency in data presentation.

## Як це зробити:
```php
<?php
$lowercaseStr = 'це приклад рядка';
$uppercaseStr = mb_strtoupper($lowercaseStr, 'UTF-8');

echo $uppercaseStr; // Виведе: 'ЦЕ ПРИКЛАД РЯДКА'
?>
```
Simple and works for most cases. Here's another method for just the first letter:
```php
<?php
$lowercaseStr = 'київ';
$capitalizedStr = mb_convert_case($lowercaseStr, MB_CASE_TITLE, "UTF-8");

echo $capitalizedStr; // Виведе: 'Київ'
?>
```

## Поглиблений Розбір
Back in the PHP 4 days, `strtoupper()` was your go-to. But with PHP 5+, `mb_strtoupper()` became crucial for multibyte strings, like those with Ukrainian characters.

Why multiple functions? `strtoupper()` struggles with non-English alphabets. `mb_strtoupper()` steps up, handling the nuances of different encodings, like UTF-8.

Under the hood, `mb_strtoupper()` respects the character encoding parameter, ensuring that capitalization works universally. This isn't just about going big on all characters. It's also about preserving the integrity of the data.

Alternatives like `mb_convert_case()` allow for more nuanced changes, like capitalizing just the first letter of each word (title case), which is particularly handy for names or titles.

## Дивіться також
- PHP Manual on String Functions: [php.net/manual/en/ref.strings.php](https://www.php.net/manual/en/ref.strings.php)
- PHP Multibyte String Functions: [php.net/manual/en/ref.mbstring.php](https://www.php.net/manual/en/ref.mbstring.php)
- UTF-8 Encoding and PHP: [php.net/manual/en/mbstring.supported-encodings.php](https://www.php.net/manual/en/mbstring.supported-encodings.php)
