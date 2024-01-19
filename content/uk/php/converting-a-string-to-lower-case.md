---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і Навіщо? 

Перетворення рядка в нижній регістр — це процес, коли всі великі букви в рядку замінюються на малі. Програмісти роблять це, щоб полегшити процес порівняння рядків, пошуку, сортування та інших операцій.

## Як це зробити:

Щоб перетворити рядок на нижній регістр в PHP, використовуйте вбудовану функцію ```strtolower()```.

```PHP
$text = "Hello, World!";
$lowercase_text = strtolower($text);
echo $lowercase_text;
```
Виходом цього кода буде:


```PHP
hello, world!
```

## Поглиблений огляд

1. Історичний контекст: Функція ```strtolower()``` присутня в PHP вже багато версій. Вона була полегшеною альтернативою ручного перетворення рядків в нижній регістр.
   
2. Альтернативи: В PHP є і інші функції для роботи з рядками. Наприклад, ```mb_strtolower()```, яка підтримує більше символів, але потребує більше ресурсів.

3. Деталі виконання: Функція ```strtolower()``` працює шляхом перевірки кожного символу в рядку. Якщо символ — літера верхнього регістру, він замінюється відповідною літерою нижнього регістру. Ця функція не змінює вхідний рядок, а повертає новий перетворений рядок.

## Дивіться також

1. [PHP: strtolower - Manual](https://www.php.net/manual/en/function.strtolower.php)
2. [PHP: mb_strtolower - Manual](https://www.php.net/manual/en/function.mb-strtolower.php)
3. [Comparison of PHP string functions](https://www.geeksforgeeks.org/comparison-of-php-string-functions/)