---
title:                "Преобразование строки в нижний регистр"
date:                  2024-01-28T23:56:41.061071-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

В PHP преобразование строки в нижний регистр означает изменение всех алфавитных символов в строке на их вариант в нижнем регистре. Программисты делают это для обеспечения консистентности, особенно при сравнении или сортировке строк, где учет регистра может все испортить.

## Как это сделать:

PHP использует `strtolower` для преобразования всех символов строки в нижний регистр. Вот как это работает:

```php
<?php
$originalString = "HeLLo WoRLD!";
$lowerCaseString = strtolower($originalString);

echo $lowerCaseString; // Выводит: hello world!
?>
```

Если вам нужно обрабатывать многобайтовые кодировки символов, такие как UTF-8, используйте вместо этого `mb_strtolower`:

```php
<?php
$originalString = "İstanbul";
$lowerCaseString = mb_strtolower($originalString, 'UTF-8');

echo $lowerCaseString; // Выводит: istanbul (правильно конвертирует İ в i)
?>
```

## Углубленно

Исторически функция `strtolower` в PHP была функцией по умолчанию для преобразования регистра, введенная в очень ранних версиях PHP. Однако, поскольку приложения PHP становились более глобальными, возникла необходимость корректно обрабатывать многобайтовые кодировки символов, что привело к появлению `mb_strtolower`.

Альтернативы `strtolower` и `mb_strtolower` включают использование регулярных выражений с функциями `mb_ereg_replace_callback` или `preg_replace_callback`, но для простого преобразования регистра они являются излишними.

В PHP строки традиционно были основаны на байтах, а не на символах, что означает, что каждый байт является одним символом. Это работает для однобайтовых кодировок, таких как ASCII, где каждый символ действительно является одним байтом. Для многобайтовых кодировок `mb_strtolower` понимает кодировку символов и обрабатывает символы так, как это должно быть.

## Смотрите также

- Руководство по PHP на `strtolower`: https://www.php.net/manual/ru/function.strtolower.php
- Руководство по PHP на `mb_strtolower`: https://www.php.net/manual/ru/function.mb-strtolower.php
- UTF-8 и Unicode для PHP-разработчиков: https://www.php.net/manual/ru/book.mbstring.php
