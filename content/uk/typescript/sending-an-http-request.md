---
title:                "Надсилання HTTP-запиту"
html_title:           "TypeScript: Надсилання HTTP-запиту"
simple_title:         "Надсилання HTTP-запиту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Що & Чому?
 Висилання HTTP запиту - це процес взаємодії між програмою та веб-сервером. Програмісті надсилають HTTP запити для отримання інформації з серверу або виконання певних дій на сервері.

 # Як: 
 Приклад коду для відправлення GET запиту:
 ```TypeScript 
 axios.get("https://example.com")
    .then(response => {
        console.log(response.data);
    })
    .catch(error => {
        console.log(error);
    })

/* Виводить в консоль відповідь від серверу. */
```

Приклад коду для відправлення POST запиту:
```TypeScript
axios.post("https://example.com", {name: "John"})
    .then(response => {
        console.log(response.data);
    })
    .catch(error => {
        console.log(error);
    })

/* Виводить в консоль відповідь від серверу, включаючи дані, які були надіслані. */
```

 # Глибше волю:
 Висилання HTTP запиту є важливою частиною розробки додатків та веб-сайтів, оскільки дозволяє обмінюватися інформацією з сервером. Цей процес став можливим завдяки створенню протоколу HTTP в 1991 році. Існують інші способи взаємодії з сервером, такі як WebSocket та GraphQL, але HTTP залишається найбільш популярним і простим для використання.

При відправленні HTTP запиту, програма спочатку створює об'єкт запиту з необхідною інформацією, такою як URL та тип запиту (GET, POST, PUT, DELETE тощо). Потім запит надсилається на сервер за допомогою мережевої бібліотеки або вбудованої функції. Після цього, сервер обробляє запит та повертає відповідь з необхідними даними або повідомленням про помилку. 

 # Дивіться також:
 Детальніше про стандартну бібліотеку TypeScript для взаємодії з сервером можна дізнатися з документації: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#support-for-old-apis. Також, ви можете вивчати інші способи роботи з сервером, наприклад, за допомогою WebSocket, у офіційній документації: https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API.