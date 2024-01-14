---
title:                "PHP: Аналізування HTML"
simple_title:         "Аналізування HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Парсинг HTML є необхідною задачею для багатьох веб-розробників. Він дозволяє отримати необхідні дані з веб-сторінок і використовувати їх для створення власних додатків або зберігання для подальшого використання.

## Як

```PHP
<?php 
// підключення бібліотеки для парсингу HTML
include_once('simple_html_dom.php');

// зазначення URL-адреси сторінки, яку потрібно спарсити
$url = 'https://example.com/';

// створення об'єкта з HTML-кодом сторінки
$html = file_get_html($url);

// знаходження елемента з відповідним селектором та виведення його тексту
$name = $html->find('h1[class="name"]', 0)->plaintext;

// виведення результату
echo "Назва сторінки: " . $name;

// звільнення пам'яті
$html->clear();
```

**Вихід:**

```
Назва сторінки: Прикладні застосунки
```

## Deep Dive

При парсингу HTML важливо знати основні принципи роботи з DOM (Document Object Model) та використовувати правильні селектори для отримання необхідних елементів. Також варто звернути увагу на обробку помилок та використання кешування для покращення продуктивності.

## Дивитись також

- [Офіційна документація PHP для DOM](https://www.php.net/manual/ru/class.domdocument.php)
- [Бібліотека simple_html_dom для парсингу HTML](https://simplehtmldom.sourceforge.io/)