---
title:                "Javascript: Надсилання запиту http з основною аутентифікацією"
simple_title:         "Надсилання запиту http з основною аутентифікацією"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP запиту з основною аутентифікацією може бути необхідним для доступу до певних ресурсів чи для ідентифікації користувача. Цей метод аутентифікації є простим та досить поширеним у веб-програмуванні, тому варто знати як його застосовувати у своїх проєктах.

## Як

Для надсилання HTTP запиту з основною аутентифікацією використовуються заголовок `Authorization` та базовий кодувальник. Перш за все, потрібно створити змінні з логіном та паролем, які будуть використовуватися для аутентифікації. Далі, у коді виконується запит за допомогою `XMLHttpRequest` або `fetch` методом, до якого додається заголовок `Authorization`. Заголовок повинен містити текст `"Basic"`, після якого слід пробіл та базовий кодувальник зі змінними логіну та пароля, перекодованими у Base64 форматі за допомогою методу `btoa()`.

```Javascript

// створення змінних з логіном та паролем
let username = 'ivan';
let password = '12345';

// створення заголовка з авторизаційними даними
let authHeader = "Basic " + btoa(username + ":" + password);

// відправка HTTP запиту з заголовком авторизації
let xhr = new XMLHttpRequest();
xhr.open('GET', 'https://example.com');
xhr.setRequestHeader('Authorization', authHeader);
xhr.send();

// або за допомогою fetch
fetch('https://example.com', {
    headers: {
        'Authorization': authHeader
    }
});
```

Після виконання запиту, в браузері буде видно відповідь від сервера, до якої можуть бути доступні ресурси згідно з авторизаційними даними.

## Глибоке дослідження

Основна аутентифікація базується на передачі авторизаційних даних у відкритому вигляді, тому не вважається безпечною. Щоб забезпечити більшу безпеку, рекомендується використовувати HTTPS замість HTTP протоколу, щоб дані передавалися у зашифрованому вигляді. Крім того, є інші методи аутентифікації, які є більш безпечними та розширеними, наприклад, використання токенів або OAuth.

## Дивись також

- [Метод `btoa()`](https://developer.mozilla.org/uk/docs/Web/API/WindowOrWorkerGlobalScope/btoa)
- [HTTP запити з на Javascript](https://developer.mozilla.org/uk/docs/Web/HTTP/Methods)
- [Основна аутентифікація](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication#Basic_authentication_scheme)