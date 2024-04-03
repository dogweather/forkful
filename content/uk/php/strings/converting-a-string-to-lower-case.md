---
date: 2024-01-20 17:39:16.854172-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-03-13T22:44:49.409115-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

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
