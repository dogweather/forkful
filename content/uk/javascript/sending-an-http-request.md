---
title:                "Javascript: Надсилання запиту http"
simple_title:         "Надсилання запиту http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP-запитів є необхідною складовою частиною багатьох веб-додатків. Завдяки цьому можливо отримати дані з веб-сервера і взаємодіяти з ним. Це дозволяє забезпечити більш динамічний та інтерактивний досвід для користувачів.

## Як до цього дійти

Надсилання HTTP-запитів можна виконати за допомогою вбудованого об'єкта `XMLHttpRequest` або з використанням `fetch` API. Нижче ми надаємо кодові приклади для обох варіантів.

```Javascript
// Використання об'єкта XMLHttpRequest
let request = new XMLHttpRequest();
request.open('GET', 'https://example.com/api/users');
request.send();
request.onload = function() {
  if (request.status === 200) {
    console.log(request.response); // виводимо дані з веб-сервера
  } else {
    console.log('Помилка! ' + request.statusText);
  }
}

// Використання fetch API
fetch('https://example.com/api/users')
  .then(response => response.json())
  .then(data => console.log(data)); // приводимо отримані дані до формату JSON і виводимо їх
```

Результатом цього коду буде виведення отриманих даних або помилки у випадку, якщо запит був неуспішним.

## Глибока пірнання

Надсилання HTTP-запитів включає в себе створення нового об'єкта `XMLHttpRequest` або використання `fetch` API. Цей об'єкт надсилає запит на веб-сервер і дозволяє отримати дані, включаючи статус запиту, заголовки та вміст. Щоб зрозуміти процес надсилання запиту більш детально, варто ознайомитися з протоколом HTTP та його різними методами, такими як GET, POST, PUT, DELETE тощо.

## Дивіться також

- [Протокол HTTP на MDN](https://developer.mozilla.org/uk/docs/Web/HTTP)
- [Об'єкт XMLHttpRequest на MDN](https://developer.mozilla.org/uk/docs/Web/API/XMLHttpRequest)
- [fetch API на MDN](https://developer.mozilla.org/uk/docs/Web/API/Fetch_API)