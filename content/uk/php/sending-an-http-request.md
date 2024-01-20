---
title:                "Відправлення HTTP-запиту"
html_title:           "Bash: Відправлення HTTP-запиту"
simple_title:         "Відправлення HTTP-запиту"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що & Для чого?

Надсилання HTTP-запиту - це процес, під час якого ваша програма просить веб-сервер надати певні дані. Програмісти роблять це, щоб взаємодіяти з API, робити web scraping, перевіряти статуси веб-сайтів і багато іншого.

## Як це зробити:

Тут є шматки коду PHP для надсилання HTTP-запиту, використовуючи `file_get_contents` і cURL.

З `file_get_contents`:

```PHP
$website = 'http://example.com';
$response = file_get_contents($website);
echo $response;
```
Результат буде HTML код головної сторінки `example.com`.

Потім cURL:

```PHP
$website = 'http://example.com';
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $website);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);
curl_close($ch);
echo $response;
```
Результат буде такий же: HTML код головної сторінки `example.com`.

## Поглиблений погляд:

На ранніх стадіях інтернету для доступу до веб-серверів використовувались лише програми-браузери. Але з розширенням можливостей веб-додатків, стало необхідним надсилати HTTP-запити прямо з програмного коду. Це привело до появи CURL і бібліотек подібних до PHP Streams (`file_get_contents`). 

Альтернативи, такі як GuzzleHTTP і PHP HTTP є більш сучасніми і гнучкими, але cURL і `file_get_contents`ще широко використовуються. 

Ключ до розуміння надіслання HTTP-запитів полягає в тому, що ви взаємодієте з сервером на інший кінець. Ви просите інформацію (GET запит) або відправляєте дані (POST, PUT, DELETE я т.д), і сервер реагує на відповідний спосіб.

## Див. також:

1. [Документація PHP по `file_get_contents`](https://www.php.net/manual/en/function.file-get-contents.php)
2. [Документація PHP по `cURL`](https://www.php.net/manual/en/book.curl.php)
3. [Guzzle, PHP HTTP client](https://docs.guzzlephp.org/en/stable/)