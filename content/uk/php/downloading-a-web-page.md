---
title:                "Завантаження веб-сторінки"
html_title:           "PHP: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і З чого це?

Завантаження веб-сторінки - це процес отримання інформації з інтернет-ресурсу на свій комп'ютер. Програмісти можуть використовувати цю функцію для отримання інформації з інших веб-сторінок, яка їм може знадобитися, наприклад, для обробки і подальшого використання.

## Як це робити

```PHP
<?php
// Використовуємо функцію file_get_contents для завантаження вмісту вказаної URL-адреси
$page_content = file_get_contents('https://example.com');
// Виводимо отриманий вміст
echo $page_content;
?>
```
**Вихід:**
```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.</p>
<p><a href="https://www.iana.org/domains/example">More information...</a></p>
</body>
</html>
```

## Глибокий занурення

Функція file_get_contents була введена в PHP версії 4 і з того часу є одним з найбільш популярних способів завантаження веб-сторінок. Альтернативою цьому може бути використання CURL (клієнтська бібліотека для передачі даних по мережі) або використання бібліотеки Simple HTML DOM для отримання певної інформації з веб-сторінки. При використанні функції file_get_contents слід враховувати можливість появи помилок або невірного вмісту від веб-сторінки, яку ми завантажуємо.

## Дивіться також

- [Офіційна документація PHP для функції file_get_contents](https://www.php.net/manual/en/function.file-get-contents.php)
- [Керування HTTP-запитами в PHP з використанням CURL](https://www.php.net/manual/en/book.curl.php)
- [Офіційна документація Simple HTML DOM бібліотеки](http://simplehtmldom.sourceforge.net/)