---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і чому?

Видобування підрядків в PHP - це процес отримання конкретної частини рядка. Програмісти роблять це для аналізу та маніпуляції з даними в рядках.

## Як робити:

Ось декілька прикладів коду PHP для видобування підрядків.

```PHP
<?php
$string = 'Привіт, світ!';
$substring = substr($string, 0, 6);

echo $substring;
// Виведе: 'Привіт'
?>
```
Вищенаведений код витягує перші 6 символів з рядка `$string`. Нижче код витягує останні 5 символів:

```PHP
<?php 
$string = 'Київ - столиця України';
$substring = substr($string, -5);

echo $substring;
// Виведе: 'раїни'
?>
```
## Поглиблений огляд:

Підфункція `substr` нещодавно прийшла до PHP, але вона вже є фундаментальною для обробки рядків. Є інші функції, такі як `strpos` та `str_replace`, які також можуть використовуватися для обробки рядків, але вони не такі гнучкі, як `substr`.

Альтернативною можливістю є `mb_substr`, яка ефективніше працює з многобайтовими символами, створеними UTF-8.

## Дивіться ще:

1. [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
2. [PHP: Mb_substr - Manual](https://www.php.net/manual/en/function.mb-substr.php)
3. [PHP: Strpos - Manual](https://www.php.net/manual/en/function.strpos.php)
4. [PHP: Str_replace - Manual](https://www.php.net/manual/en/function.str-replace.php)