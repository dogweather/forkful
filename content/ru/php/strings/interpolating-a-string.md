---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:22.682456-07:00
description: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F\
  \ \u0441\u0442\u0440\u043E\u043A \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\
  \u0442 \u043D\u0430\u043F\u0440\u044F\u043C\u0443\u044E \u0432\u0441\u0442\u0430\
  \u0432\u043B\u044F\u0442\u044C \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F\
  \ \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0445 \u0432 \u0441\u0442\
  \u0440\u043E\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F \u0438\u043D\u0442\u0435\u0433\u0440\u0430\
  \u0446\u0438\u0438 \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0445\
  \ \u0432 \u0442\u0435\u043A\u0441\u0442, \u0434\u0435\u043B\u0430\u044F \u043A\u043E\
  \u0434\u2026"
lastmod: '2024-03-13T22:44:45.184187-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F\
  \ \u0441\u0442\u0440\u043E\u043A \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\
  \u0442 \u043D\u0430\u043F\u0440\u044F\u043C\u0443\u044E \u0432\u0441\u0442\u0430\
  \u0432\u043B\u044F\u0442\u044C \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F\
  \ \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0445 \u0432 \u0441\u0442\
  \u0440\u043E\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F \u0438\u043D\u0442\u0435\u0433\u0440\u0430\
  \u0446\u0438\u0438 \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0445\
  \ \u0432 \u0442\u0435\u043A\u0441\u0442, \u0434\u0435\u043B\u0430\u044F \u043A\u043E\
  \u0434\u2026"
title: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F \u0441\
  \u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и почему?

Интерполяция строк позволяет напрямую вставлять значения переменных в строку. Программисты используют это для интеграции переменных в текст, делая код чище и более читабельным.

## Как это сделать:

В PHP можно интерполировать строки, используя двойные кавычки или синтаксис heredoc:

```php
$name = "World";
echo "Привет, $name!"; // Вывод: Привет, World!

// Использование фигурных скобок для более сложных переменных
$object = new stdClass();
$object->greeting = "Привет";
echo "{$object->greeting}, $name!"; // Вывод: Привет, World!

// Синтаксис heredoc для многострочных строк
$heredoc = <<<EOT
Это строка, содержащая $name внутри нее.
Здесь вы можете писать столько, сколько хотите.
EOT;
echo $heredoc; // Вывод: Это строка, содержащая World внутри нее.
```

Заметьте: Одинарные кавычки не интерполируются:

```php
echo 'Привет, $name!'; // Вывод: Привет, $name!
```

## Погружение

До введения интерполяции в PHP, конкатенация с оператором точка (.) была обычным делом. Например:

```php
echo 'Привет, ' . $name . '!';
```

Интерполяция упрощает этот процесс, позволяя парсить переменную непосредственно внутри строки.

Интерполяция строк существует с PHP 4, но использование сложных выражений внутри фигурных скобок стало более гибким с PHP 7. С этими улучшениями PHP облегчил встраивание любой переменной, включая свойства объектов и элементы массива, в строку.

Существуют альтернативы интерполяции, такие как использование `sprintf()` для форматированных строк или `implode()` для массивов. Иногда они могут предложить больший контроль над форматированием строк, особенно для локализации и сложных структур.

С точки зрения реализации, PHP ищет переменные внутри строк, когда они находятся в двойных кавычках или синтаксисе heredoc, и заменяет их значением переменной. Парсер игнорирует знак доллара ($) в строках в одинарных кавычках, обрабатывая его как обычный символ.

## Смотрите также

- [PHP: Строки](http://php.net/manual/en/language.types.string.php) - Официальная документация PHP по строкам.
- [PHP: Синтаксис Heredoc](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) - Подробный раздел руководства PHP по Heredoc.
- [PHP: Операторы строки](https://www.php.net/manual/en/language.operators.string.php) - Больше о конкатенации строк и операторе точка.
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php) - Документация функции `sprintf()` для форматирования строк.
