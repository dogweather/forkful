---
title:                "Надіслання запиту http"
html_title:           "Javascript: Надіслання запиту http"
simple_title:         "Надіслання запиту http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і для чого?

Відправка HTTP запиту - це процес передачі запиту веб-ресурсу. Програмісти використовують їх для взаємодії з сервером та отримання відповіді на запитання, що були поставлені.

## Як:

```javascript
// Приклад використання бібліотеки Axios для відправлення GET запиту
axios.get('https://example.com')
  .then(function(response){
    console.log('Отримано відповідь від серверу:', response);
  })
  .catch(function(error){
    console.log('Сталася помилка при відправленні запиту:', error);
  });

// Вивід:
// Отримано відповідь від серверу: {status: 200, data: 'Привіт, світ!'}
```

## Глибше:

Відправлення HTTP запиту - це важлива частина розробки веб-додатків, яка дозволяє взаємодіяти з сервером та отримувати потрібні дані. Існують різні альтернативи для відправки таких запитів, такі як Fetch API або вбудований модуль в Node.js під назвою http. Відправка запиту може бути реалізована за допомогою різних методів, наприклад GET, POST, PUT, DELETE.

## Дивіться також:

- [Fetch API](https://developer.mozilla.org/uk/docs/Web/API/Fetch_API)
- [Приклади використання Axios](https://github.com/axios/axios#example)