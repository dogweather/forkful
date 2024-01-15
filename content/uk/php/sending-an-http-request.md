---
title:                "Відправлення http запиту"
html_title:           "PHP: Відправлення http запиту"
simple_title:         "Відправлення http запиту"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Причина

Надсилання HTTP-запиту - це необхідна процедура для отримання даних з веб-сайту або веб-сервера. Використовуючи PHP, ви можете здійснювати HTTP-запити і отримувати потрібну інформацію, яка допоможе у вашому проекті або дослідженні.

## Як

Для відправлення HTTP-запиту за допомогою PHP необхідно використовувати функцію `file_get_contents()` з вказаною URL-адресою. Наприклад, якщо вам потрібно отримати вміст сторінки Google, ви можете скористатися наступним кодом:

```PHP
$contents = file_get_contents("https://www.google.com");
echo $contents;
```

Ви отримаєте вміст сторінки Google у вигляді HTML-коду. Ви також можете використовувати функцію `curl` для відправлення HTTP-запиту та отримання розширених відповідей, таких як заголовки тощо.

```PHP
$ch = curl_init("https://www.google.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);
curl_close($ch);
echo $response;
```

Також можна встановити додаткові параметри для HTTP-запиту з використанням функцій `file_get_contents()` та `curl`.

## Deep Dive

При використанні `file_get_contents()`, PHP автоматично встановлює заголовки `User-Agent` та `Accept-Encoding`, що можуть бути потрібні для успішного виконання HTTP-запиту. За необхідності, їх можна змінити шляхом використання параметрів `stream_context_create()`.

Крім того, ви можете використовувати різні методи HTTP для надсилання запиту, наприклад, `GET`, `POST`, `PUT`, `DELETE` тощо. Також є можливість передавати дані у вигляді параметрів через URL-адресу або у тілі запиту.

Якщо ви впевнені, що ваш сайт підтримує HTTPS, то слід втримуватися від використання `file_get_contents()` і використовувати `curl` з належним встановленням параметрів для безпечної передачі даних.

## Дивись також

- [Функція file_get_contents() у документації PHP](https://www.php.net/manual/en/function.file-get-contents.php)
- [Функція curl у документації PHP](https://www.php.net/manual/en/function.curl.php)
- [Основи HTTP-запитів у документації W3Schools](https://www.w3schools.com/whatis/whatis_http.asp)