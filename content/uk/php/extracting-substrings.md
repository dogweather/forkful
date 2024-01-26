---
title:                "Виділення підрядків"
date:                  2024-01-20T17:46:33.827630-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Що таке і навіщо?)
Просто кажучи, витягування підрядків – це процес отримання частини тексту з більшої стрічки. Програмісти роблять це, щоб обробити, перевірити, чи відобразити тільки ту інформацію, яка їм потрібна.

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
