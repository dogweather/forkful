---
title:                "Перетворення рядка у нижній регістр"
aliases: - /uk/php/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:16.854172-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Приведення рядків до нижнього регістру означає заміну усіх великих літер у рядку на малі. Це робиться для уніфікації даних, наприклад, при порівнянні email адрес або коли система нечутлива до регістру літер.

## How to: (Як це зробити:)
```PHP
<?php 
$string = "Привіт, Світ!";
$lowercaseString = mb_strtolower($string, 'UTF-8');
echo $lowercaseString; // виведе "привіт, світ!"
?>
```
Функція `mb_strtolower` гарантує, що всі символи, включаючи не-ASCII, будуть правильно переведені до нижнього регістру, важливо при використанні UTF-8.

## Deep Dive (Поглиблений огляд)
Раніше, у PHP, для роботи з рядками використовувалась функція `strtolower`, яка працювала добре з ASCII символами, але могла викликати проблеми з іншими алфавітами. `mb_strtolower` - частина більш широкої mbstring (multibyte string) бібліотеки, яка забезпечує кращу підтримку Unicode, важлива для багатомовних середовищ, як Україна.

Також існують альтернативи `mb_strtolower`, наприклад, `mb_convert_case`, який пропонує більш гранульований контроль над перетворенням регістру. У вас є можливість вибрати режим перетворення.

Для уникнення несподіваних результатів варто вказувати кодування, в даному випадку 'UTF-8'. Це гарантує, що функція впорається з нестандартними символами та іншомовними рядками правильно.

## See Also (Дивіться також)
- [PHP Manual on mb_strtolower](https://www.php.net/manual/en/function.mb-strtolower.php)
- [PHP Manual on mb_convert_case](https://www.php.net/manual/en/function.mb-convert-case.php)
- [Unicode Consortium](https://home.unicode.org/)
