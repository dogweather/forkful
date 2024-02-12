---
title:                "Поиск и замена текста"
aliases:
- ru/php/searching-and-replacing-text.md
date:                  2024-01-29T00:02:36.732896-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Поиск и замена текста - это метод поиска определенных строк в контенте и их замена на что-то другое. Программисты делают это для обновления данных, исправления ошибок или массового изменения текста без ручных правок.

## Как это сделать:

Вот быстрый способ заменить 'cat' на 'dog' в предложении с использованием PHP:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat';
$replacedText = str_replace('cat', 'dog', $text);

echo $replacedText;
?>
```

Пример вывода:

```
The quick brown fox jumps over the lazy dog
```

Теперь предположим, что нам нужно сделать замену без учета регистра:

```PHP
<?php
$text = 'Catapults are CATegorically amazing!';
$replacedText = str_ireplace('cat', 'dog', $text);

echo $replacedText;
?>
```

Пример вывода:

```
Dogapults are DOGegorically amazing!
```

## Погружение:

Функции поиска и замены существуют с первых дней компьютерной эры — вспомните `sed` в Unix. В PHP `str_replace` и `str_ireplace` являются основными функциями для простого поиска и замены. `str_replace` учитывает регистр, в то время как `str_ireplace` - нет.

Как они работают? Внутри обе функции проверяют каждую часть строки, ищут совпадения и заменяют их. Они также работают с массивами, так что вы можете искать и заменять несколько шаблонов за один раз.

Теперь, если вам нужен больший контроль, например, сопоставление по шаблону, вы захотите использовать `preg_replace`. Это использует регулярные выражения, предлагая гораздо большую гибкость и точность:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat 7 times.';
$replacedText = preg_replace('/\bcat\b/i', 'dog', $text);

echo $replacedText;
?>
```

Пример вывода:

```
The quick brown fox jumps over the lazy dog 7 times.
```

Это замена 'cat' на 'dog', без учета регистра (модификатор `/i`), и с учетом только целых слов (`\b` граница слова).

## Смотрите также:

- Официальная документация PHP по str_replace: https://www.php.net/manual/ru/function.str-replace.php
- Официальная документация PHP по str_ireplace: https://www.php.net/manual/ru/function.str-ireplace.php
- Официальная документация PHP по preg_replace: https://www.php.net/manual/ru/function.preg-replace.php
- Учебник по регулярным выражениям: https://www.regular-expressions.info/
- Редактор потоков `sed` в Unix для фильтрации и преобразования текста: http://www.grymoire.com/Unix/Sed.html
