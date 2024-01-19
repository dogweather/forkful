---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Завантаження веб-сторінки - це процес отримання HTML коду веб-сторінки. Програмісти це роблять для аналізу даних, тестування або веб-скрапінгу.

## Як це зробити:

В PHP (версії 7 і вище) для завантаження веб-сторінки можна використовувати функцію `file_get_contents()`. Нижче наведений приклад:

```PHP
<?php
$url = 'http://example.com';
$content = file_get_contents($url);
echo $content;
?>
```

Запустівши цей код, ви отримаєте HTML-код сторінки `http://example.com`.

## Більш глибоке занурення:

Функція `file_get_contents()` була представлена в PHP 4.3.0, а отже, є однією зі стандартних можливостей PHP для роботи з веб-сторінками.

Іншою популярною заміною є `cURL`, що надає більше можливостей для конфігурації і краще підходить для складних випадків.

```PHP
<?php
$url = 'http://example.com';
$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$result = curl_exec($ch);
curl_close($ch);
echo $result;
?>
```

Особливість завантаження сторінок в PHP в тому, що воно працює на сервері, а не на клієнтській стороні, що дає передбачувані результати без залежності від браузера клієнта.

## Див. також:

1. [Документація PHP на `file_get_contents()`](http://php.net/manual/ru/function.file-get-contents.php)

2. [Документація PHP на `cURL`](https://www.php.net/manual/ru/book.curl.php)

3. [Курси про PHP на Codecademy](https://www.codecademy.com/learn/learn-php)