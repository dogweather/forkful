---
title:                "Перетворення рядка в нижній регістр"
html_title:           "PHP: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Існують різні ситуації, коли потрібно перетворити текстовий рядок на рядок з малими літерами. Наприклад, для нормалізації даних або для зручності пошуку. 

## How To

Щоб перетворити рядок на рядок з малими літерами використовують функцію `strtolower()`. Наприклад:

```PHP
$text = "ЦЕ РЯДОК З ВЕЛИКИМИ ЛІТЕРАМИ";
$lowercase_text = strtolower($text);
echo $lowercase_text;
```

Результат:

```
це рядок з великими літерами
```

## Deep Dive

Щоб бути точними, `strtolower()` перетворює всі літери в рядку на їхні нижні аналоги, згідно з поточним регіональними налаштуванням. Також вона враховує особливості кожної мови, наприклад, перетворює літери з діакритичними знаками у прості літери.

Навіть якщо ви працюєте з українським текстом, все одно краще використовувати `strtolower()`, оскільки це гарантує коректне перетворення всіх символів, навіть тих, що мають діакритичні знаки.

## See Also

- [PHP: strtolower - Manual](https://www.php.net/manual/en/function.strtolower.php)
- [Unicode Character Database - Unicode Consortium](https://www.unicode.org/ucd/)
- [PHP String Functions - W3Schools](https://www.w3schools.com/php/php_ref_string.asp)