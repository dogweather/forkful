---
title:                "PHP: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки є одним з найпоширеніших завдань у веб-програмуванні. Це дозволяє отримати доступ до інформації, яка може бути використана для створення динамічного веб-сайту, або просто для отримання необхідних даних для аналізу.

## Як

Для завантаження веб-сторінки використовується PHP функція `file_get_contents()`. Наприклад, якщо у нас є потрібна нам адреса веб-сторінки, ми можемо зберегти її в змінну `$url` і застосувати функцію `file_get_contents()`:

```PHP
$url = "https://example.com/";
$html = file_get_contents($url);
echo $html;
```

Результатом буде HTML код сторінки, який потім можна обробити або відобразити на сайті.

## Deep Dive

Функція `file_get_contents()` є потужним інструментом, оскільки дозволяє не тільки отримати вміст веб-сторінки, але і передати параметри запиту, які можуть бути корисними для роботи з API або іншими веб-сервісами.

Наприклад, якщо ми хочемо отримати JSON формат даних, ми можемо передати параметр `"content-type:application/json"` у другому аргументі функції `file_get_contents()`:

```PHP
$url = "https://example.com/api/";
$data = array("name" => "John Doe", "email" => "john@example.com");
$options = array(
    "http" => array(
        "header" => "Content-Type:application/json",
        "method" => "POST",
        "content" => json_encode($data),
    ),
);
$html = file_get_contents($url, false, stream_context_create($options));
echo $html;
```

Отриманий результат буде у форматі JSON, який потім можна обробити у PHP або використати для будь-яких інших цілей.

## Дивитися також

- [Офіційна документація PHP для функції `file_get_contents()`](https://www.php.net/manual/en/function.file-get-contents.php)
- [Стаття про роботу з JSON в PHP](https://www.codecademy.com/learn/make-a-website/modules/apis/cheatsheet)
- [Приклад використання `file_get_contents()` для завантаження вмісту веб-сторінки](https://codeseekah.com/2012/10/19/why-you-cant-standalone-code-after-all/)