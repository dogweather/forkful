---
title:                "PHP: Перетворення рядка на малі літери"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Почему

Перевод строки в нижний регистр - это необходимый навык для любого PHP-разработчика. Это позволяет вам обрабатывать данные из различных источников и использовать их в коде.

## Как

```PHP
$string = "Привет, МИР!";
echo strtolower($string); // выводит "привет, мир!"
```

Выше приведен простой пример использования функции `strtolower()`, которая переводит все символы строки в нижний регистр. Код можно использовать в любом проекте, где необходимо обработать текст и привести его к единому регистру.

## Глубокая погружение

В PHP существует несколько способов перевода строки в нижний регистр. Одним из них является функция `strtolower()`, как показано выше. Другими вариантами могут быть использование инструмента `mb_strtolower()` для работы с мультибайтовыми строками или же использование метода `toLower()` в классе `String` в PHP 7. Для более глубокого понимания различных методов перевода строки в нижний регистр, обратитесь к официальной документации PHP.

## Смотрите также

- [PHP: strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [PHP: mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)
- [PHP: String Methods](https://www.php.net/manual/en/ref.strings.php)