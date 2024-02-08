---
title:                "Преобразование строки в верхний регистр"
aliases:
- ru/php/capitalizing-a-string.md
date:                  2024-01-28T23:55:41.621337-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Приведение строки к виду с заглавными буквами означает превращение первой буквы каждого слова в заглавную. Программисты используют заглавные буквы для визуальной консистенции, брендинга или дизайна пользовательского интерфейса.

## Как это сделать:
В PHP строки приводят к виду с заглавными буквами с помощью функции `ucwords()` для полных названий или `ucfirst()` для отдельных строк или предложений.

```php
<?php
$lowercase_title = "the quick brown fox jumps over the lazy dog";
$capitalized_title = ucwords($lowercase_title);

echo $capitalized_title; // Вывод: The Quick Brown Fox Jumps Over The Lazy Dog

$sentence = "an example sentence.";
$capitalized_sentence = ucfirst($sentence);

echo $capitalized_sentence; // Вывод: An example sentence.
?>
```

## Глубокое погружение
Приведение строк к виду с заглавными буквами - не новая концепция. В мире печати использование заглавных букв в названиях является стандартной конвенцией. В PHP функции `ucwords` и `ucfirst` существуют уже довольно давно, поддерживая такие конвенции в цифровом виде. Функция `mb_convert_case` PHP позволяет выполнять более сложные манипуляции, например, `MB_CASE_TITLE`, что особенно полезно для многобайтовых (не ASCII) строк.

Альтернативы `ucwords` включают в себя `strtoupper`, которая преобразует всю строку в верхний регистр, и `strtolower`, которая делает строку нижнего регистра. Следует учитывать локализацию: в некоторых языках существуют уникальные правила для заглавных букв.

С точки зрения реализации, `ucwords` применяет преобразование в верхний регистр к первому символу после любого пробельного символа, не только после пробелов. Это значит, что новые строки, табуляции и т. д. также инициируют преобразование.

## См. также
Для получения дополнительной информации посмотрите:

- Руководство по PHP о `ucwords()`: https://www.php.net/manual/ru/function.ucwords.php
- Руководство по PHP о `ucfirst()`: https://www.php.net/manual/ru/function.ucfirst.php
- Руководство по PHP о `mb_convert_case()`: https://www.php.net/manual/ru/function.mb-convert-case.php
- Функции для работы со строками в PHP: https://www.php.net/manual/ru/ref.strings.php
