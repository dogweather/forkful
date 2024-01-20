---
title:                "Переведення рядка в верхній регістр"
html_title:           "PHP: Переведення рядка в верхній регістр"
simple_title:         "Переведення рядка в верхній регістр"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Трансформація рядка у верхній регістр - це процесс перетворення всіх маленьких літер великими. Програмісти часто користуються цим, щоб створити рядки-константи або зробити текст більш зрозумілим для користувача.

## Як це зробити:

PHP має вбудовану функцію `strtoupper()` для такої задачі. Ось приклад:

```PHP
<?php
$text = "привіт, світ!";
$capsText = strtoupper($text);
echo $capsText;
?>
```

Цей код виведе: "ПРИВІТ, СВІТ!".

## Поглиблення у тему

Функція `strtoupper()` була частиною PHP ще з його початкової версії, PHP/FI 2.0, яка була випущена в 1997 році.

Є чимало альтернатив для трансформації рядка у верхній регістр, включаючи використання `mb_strtoupper()`, яка корисна для рядків з багатьма кодуваннями.

Невеликий момент до розуміння: `strtoupper()` та `mb_strtoupper()` працюють так, що їх виклик спершу поділяє рядок на частини або "символи" і перетворює кожний елемент окремо.

## Що ще подивитися:

Варто подивитися документацію PHP на функції [strtoupper()](https://www.php.net/manual/function.strtoupper) і [mb_strtoupper()](https://www.php.net/manual/function.mb-strtoupper), а також огляд їх використання на [Stack Overflow](https://stackoverflow.com/questions/1326672/make-a-string-uppercase-in-php).