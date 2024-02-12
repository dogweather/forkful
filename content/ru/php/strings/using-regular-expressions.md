---
title:                "Использование регулярных выражений"
aliases:
- ru/php/using-regular-expressions.md
date:                  2024-01-29T00:03:44.114834-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Регулярные выражения (regex) — это поисковые шаблоны, используемые для поиска комбинаций символов в строках. Программисты используют их для задач, таких как валидация, поиск и разбор текста, поскольку они мощные и экономят время.

## Как использовать:
Для использования regex в PHP обычно применяют `preg_match` для поиска совпадения, или `preg_replace` для поиска и замены. Вот быстрый пример:

```php
<?php
$string = "Быстрая коричневая лиса прыгает через ленивую собаку.";

// Проверяем, есть ли 'Быстрая' в строке
if (preg_match("/Быстрая/", $string)) {
  echo "Совпадение найдено!";
} else {
  echo "Совпадений не найдено.";
}
// Вывод: Совпадение найдено!

// Заменяем 'коричневая' на 'красная'
$replacedString = preg_replace("/коричневая/", "красная", $string);
echo $replacedString;
// Вывод: Быстрая красная лиса прыгает через ленивую собаку.
?>
```

## Глубокое погружение
Регулярные выражения существуют с 1950-х годов и были широко имплементированы в Perl, оказав влияние на многие другие языки, включая PHP. Альтернативы regex в PHP включают функции, такие как `strpos()` для поиска подстрок или `str_replace()` для замены текста. Библиотека PCRE (Perl Compatible Regular Expressions) используется в PHP под капотом для функций regex, предлагая богатые и мощные возможности поиска по шаблону.

## См. также
- [Официальная документация PHP по PCRE](https://www.php.net/manual/ru/book.pcre.php)
- [Regular-Expressions.info](https://www.regular-expressions.info/) - для глубокого понимания regex.
- [Regex101](https://regex101.com/) - для тестирования и отладки ваших regex-шаблонов.
