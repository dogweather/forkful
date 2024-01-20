---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому? 

Надіслати HTTP-запит із базовою аутентифікацією - це означає дозволити програмі пройти перевірку безпеки на веб-сервері. Програмісти роблять це, коли хочуть отримати доступ до захищених даних, не вимагаючи від користувача вводити логін і пароль.

## Як це зробити:

Ось основний код, який надсилає HTTP-запит з базовою аутентифікацією за допомогою функції `fetch()`:

``` Javascript
let url = 'https://example.com/data';
let username = 'your-username';
let password = 'your-password';

let headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ":" + password));

fetch(url, {method:'GET', headers: headers})
    .then(response => response.json())
    .then(console.log)
    .catch(console.error);
```

Код модифікує заголовки запиту, додаючи поле "Authorization" з інформацією про аутентифікацію.


## Більш глибокий аналіз:

1. **Історичний контекст** - було створено "Basic Authentication" з простої необхідності аутентифікації без великого перешикування даних.
2. **Альтернативи** - однак, базова аутентифікація не є самим безпечним варіантом. Для більшої безпеки користуйтеся аутентифікацією OAuth або Token.
3. **Деталі реалізації** - Основним недоліком цього методу є те, що ім'я користувача та пароль базово кодуються і можуть бути легко розшифровані. Вони надсилаються з кожним запитом, що збільшує ризик злому браузера.


## Також дивіться:

1. Загальна інформація про fetch() на MDN: [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)

2. Більше про заголовки HTTP-запитів: [HTTP headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers)

3. Детальний огляд на тему безпеки базової аутентифікації: [Basic Authentication](https://www.httpwatch.com/httpgallery/authentication/)

4. MDN розширений огляд HTTP-аутентифікації: [HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)