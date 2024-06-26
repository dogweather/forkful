---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:36.732896-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u0441\
  \u043F\u043E\u0441\u043E\u0431 \u0437\u0430\u043C\u0435\u043D\u0438\u0442\u044C\
  \ 'cat' \u043D\u0430 'dog' \u0432 \u043F\u0440\u0435\u0434\u043B\u043E\u0436\u0435\
  \u043D\u0438\u0438 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u043D\u0438\u0435\u043C PHP."
lastmod: '2024-03-13T22:44:45.182408-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u0437\u0430\u043C\u0435\u043D\u0438\u0442\u044C 'cat'\
  \ \u043D\u0430 'dog' \u0432 \u043F\u0440\u0435\u0434\u043B\u043E\u0436\u0435\u043D\
  \u0438\u0438 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u043D\u0438\u0435\u043C PHP."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

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
