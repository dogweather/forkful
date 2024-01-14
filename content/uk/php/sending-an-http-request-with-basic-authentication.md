---
title:                "PHP: Надсилання запиту http з базовою аутентифікацією."
simple_title:         "Надсилання запиту http з базовою аутентифікацією."
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

У цій статті ми розглянемо процес відправлення запиту HTTP з базовою автентифікацією в PHP. Це допоможе вам зрозуміти, як цей механізм працює та чому він важливий для вашої програми.

## Як це зробити

Для відправлення HTTP запиту з базовою автентифікацією, спочатку потрібно створити об'єкт класу `CURL` та встановити параметри для цього запиту. Далі ми вказуємо URL, до якого буде відправлятися запит, та потрібний метод, наприклад, `GET` або `POST`. Також встановлюємо параметри для автентифікації, використовуючи функцію `curl_setopt`. Нижче наведений приклад коду:

```PHP
$url = "https://example.com/api";
$curl = curl_init();

// Встановлюємо опції для CURL
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
curl_setopt($curl, CURLOPT_URL, $url);
curl_setopt($curl, CURLOPT_HTTPHEADER, ['Content-Type: application/json']);
curl_setopt($curl, CURLOPT_USERPWD, "username:password"); // Вказуємо дані для автентифікації
curl_setopt($curl, CURLOPT_CUSTOMREQUEST, 'GET'); // Вказуємо метод запиту

// Виконуємо запит та отримуємо результат
$response = curl_exec($curl);
curl_close($curl);

echo $response; // Виводимо результат
```

В результаті ви отримаєте відповідь з веб-сервера, яку можна обробити далі в своїй програмі.

## Детальний огляд

Базова автентифікація використовується для захисту веб-ресурсів від несанкціонованого доступу. Вона вимагає від користувача надати додаткові дані для підтвердження своєї ідентичності. Це зроблено за допомогою відправлення хедера `Authorization` з потрібними даними, закодованими у форматі Base64.

У PHP для реалізації базової автентифікації використовується функція `curl_setopt` з параметром `CURLOPT_USERPWD`. Вона приймає два аргументи - ім'я користувача та пароль. Ці дані будуть використовуватися для створення хедера `Authorization`.

## Дивись також

- [Документація з CURL для PHP](https://www.php.net/manual/ru/book.curl.php)
- [Приклади HTTP запитів з CURL](https://www.php.net/manual/en/curl.examples-basic.php)
- [Пояснення базової автентифікації](https://www.w3.org/Protocols/rfc2616/rfc2616-sec11.html#sec11.1)