---
date: 2024-01-20 17:46:33.827630-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) ."
lastmod: '2024-03-13T22:44:49.412435-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## How to: (Як робити:)
```php
<?php
$text = "Привіт, світ!";
// Витягування підрядка від початку
echo substr($text, 0, 7); // Виведе 'Привіт,'
// Витягування підрядка з кінця
echo substr($text, -5); // Виведе 'світ!'
?>
```

## Deep Dive (Поглиблений погляд)
Метод `substr()` в PHP використовується з часів ранніх версій мови. Як альтернатива існує `mb_substr()`, який бере до уваги багатобайтові символи, що важливо для правильної роботи з UTF-8 (наприклад, українською мовою).

У функції `substr()` три аргументи: сама строка, початковий індекс і довжина підрядка. Початковий індекс може бути негативним, що означає відлік з кінця стрічки.

## See Also (Додаткові ресурси)
- [PHP substr() function](https://www.php.net/manual/en/function.substr.php)
- [PHP mb_substr() function](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP string manipulation](https://www.php.net/manual/en/ref.strings.php)
