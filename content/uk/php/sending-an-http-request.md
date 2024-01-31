---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T18:00:23.697041-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?
HTTP-запит дозволяє вашому PHP-скрипту спілкуватися з іншими серверами. Ми робимо це для отримання та відправки даних, інтеграції з API, взаємодії із веб-сервісами.

## Як це робити:
В PHP для відправлення HTTP-запитів можна використовувати cURL або file_get_contents(). Ось приклади обох:

```PHP
// Використання cURL
$ch = curl_init('http://example.com/api');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);
curl_close($ch);
echo $response;

// Використання file_get_contents()
$response = file_get_contents('http://example.com/api');
echo $response;
```
Обидва відображають відповідь від http://example.com/api.

## Поглиблений огляд
cURL у PHP — потужна бібліотека для відправлення HTTP-запитів, підтримує багато опцій. file_get_contents() простіший, але менш гнучкий.

cURL з’явився у 1997 році, і з тих пір став стандартом для веб-запитів в PHP. file_get_contents() добре підходить для простих GET-запитів. Є й інші бібліотеки, наприклад, Guzzle, які надають більше можливостей та кращу обробку помилок.

## Див. також:
- [PHP cURL](http://php.net/manual/en/book.curl.php) - офіційна документація по cURL в PHP.
- [PHP Streams](http://php.net/manual/en/book.stream.php) - офіційна документація по потоках в PHP.
- [Guzzle](http://docs.guzzlephp.org/) - сучасний HTTP-клієнт для PHP.
