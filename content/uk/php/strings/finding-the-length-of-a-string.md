---
date: 2024-01-20 17:48:05.998240-07:00
description: "\u0429\u043E \u0442\u0430\u043A\u0435 \u0434\u043E\u0432\u0436\u0438\
  \u043D\u0430 \u0440\u044F\u0434\u043A\u0430 \u0442\u0430 \u0447\u043E\u043C\u0443\
  \ \u043C\u0438 \u0446\u0435 \u0432\u0438\u043C\u0456\u0440\u044E\u0454\u043C\u043E\
  ? \u0423 PHP, \u044F\u043A \u0456 \u0432 \u0431\u0430\u0433\u0430\u0442\u044C\u043E\
  \u0445 \u043C\u043E\u0432\u0430\u0445 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0443\u0432\u0430\u043D\u043D\u044F, \u0434\u043E\u0432\u0436\u0438\u043D\u0430\
  \ \u0440\u044F\u0434\u043A\u0430 \u2013 \u0446\u0435 \u043A\u0456\u043B\u044C\u043A\
  \u0456\u0441\u0442\u044C \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443\
  \ \u043D\u044C\u043E\u043C\u0443. \u0426\u0435 \u0432\u0430\u0436\u043B\u0438\u0432\
  \u043E \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.415856-06:00'
model: gpt-4-1106-preview
summary: "\u0429\u043E \u0442\u0430\u043A\u0435 \u0434\u043E\u0432\u0436\u0438\u043D\
  \u0430 \u0440\u044F\u0434\u043A\u0430 \u0442\u0430 \u0447\u043E\u043C\u0443 \u043C\
  \u0438 \u0446\u0435 \u0432\u0438\u043C\u0456\u0440\u044E\u0454\u043C\u043E? \u0423\
  \ PHP, \u044F\u043A \u0456 \u0432 \u0431\u0430\u0433\u0430\u0442\u044C\u043E\u0445\
  \ \u043C\u043E\u0432\u0430\u0445 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\
  \u0432\u0430\u043D\u043D\u044F, \u0434\u043E\u0432\u0436\u0438\u043D\u0430 \u0440\
  \u044F\u0434\u043A\u0430 \u2013 \u0446\u0435 \u043A\u0456\u043B\u044C\u043A\u0456\
  \u0441\u0442\u044C \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443 \u043D\
  \u044C\u043E\u043C\u0443. \u0426\u0435 \u0432\u0430\u0436\u043B\u0438\u0432\u043E\
  \ \u0434\u043B\u044F\u2026"
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## What & Why?
Що таке довжина рядка та чому ми це вимірюємо? У PHP, як і в багатьох мовах програмування, довжина рядка – це кількість символів у ньому. Це важливо для валідації введених даних, обробки тексту та збереження пам’яті.

## How to:
Знайдемо довжину рядка за допомогою функції `strlen`.
```PHP
<?php
$text = "Привіт, світ!";
echo strlen($text); // Покаже: 21
?>
```
Зверніть увагу, що в UTF-8 кожен український символ може використовувати більше одного байта.

## Deep Dive:
У минулому, до UTF-8, ми просто рахували байти. Тепер, з різноманітністю символів та ємністю, `strlen` не завжди надійна для багатобайтових кодувань. Використовуйте `mb_strlen` для точного підрахунку в UTF-8.

```PHP
<?php
echo mb_strlen($text, "UTF-8"); // Правильно покаже: 12
?>
```

Альтернативи `strlen` інколи потрібні, наприклад `substr_count` для підрахунку певних символів. На рівні реалізації, PHP оперує складніше, оскільки обробляє різні кодування та розміри символів.

## See Also:
* Документація PHP про `strlen`: https://www.php.net/manual/uk/function.strlen.php
* Документація PHP про `mb_strlen`: https://www.php.net/manual/uk/function.mb-strlen.php
* Про кодування UTF-8: https://uk.wikipedia.org/wiki/UTF-8
