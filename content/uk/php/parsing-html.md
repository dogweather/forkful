---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/parsing-html.md"
---

{{< edit_this_page >}}

## Що та чому?

Парсинг HTML - це процес, в якому програма аналізує код HTML та перетворює його на зрозумілу для PHP структуру. Програмісти роблять це, щоб зручно витягувати, маніпулювати або змінювати дані з веб-сторінок.

## Як це зробити:

Використовуючи PHP, ми можемо використовувати вбудований клас DOMDocument для парсингу HTML. Давайте розглянемо приклад:

```PHP
<?php
libxml_use_internal_errors(true);

$dom = new DOMDocument;
$dom->loadHTML('<html><body><p>Hello, World!</p></body></html>');

$p = $dom->getElementsByTagName('p')->item(0);

echo $p->textContent;
?>
```

Виходом буде: `Hello, World!`. Це те, що міститься в тегах `<p>` вашого HTML.

## Поглиблений аналіз:

1. Історичний контекст: Функціонал парсингу HTML був добавлений в PHP в версії 5.0.0, з метою фахівцям відправляти вміст веб-сторінки безпосредньо в PHP скрипт.

2. Альтернативи: Крім вбудованого класу `DOMDocument`, існують інші інструменти для парсингу HTML, як-от `SimpleXML` або бібліотеки третіх сторін, наприклад `PHPQuery` та `Goutte`.

3. Деталі реалізації: Під час парсингу HTML, DOMDocument конвертує текстовий HTML у об'єктову модель документа (DOM). Це дозволяє вам працювати з елементами HTML, як з об'єктами.

## Дивіться також:

1. [Офіційна документація PHP на DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
2. [Зрозумійте PHP SimpleXML з прикладами](https://www.w3schools.com/php/php_ref_simplexml.asp)
3. [PHPQuery: PHP бібліотека для парсингу HTML](https://code.google.com/archive/p/phpquery/)
4. [Goutte, PHP веб-скрепер та бібліотека для парсингу HTML](https://github.com/FriendsOfPHP/Goutte)