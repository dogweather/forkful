---
title:                "Надсилання http-запиту"
html_title:           "TypeScript: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Правильне відправлення HTTP-запитів є важливою частиною розробки веб-додатків. Це дає змогу обмінюватися даними між клієнтом і сервером та забезпечує швидке виконання запитів.

## Як

```TypeScript
// Приклад відправлення GET-запиту
import axios from 'axios';

axios.get('https://api.example.com/users')
  .then(response => {
    // Обробка отриманих даних
    console.log(response.data);
  })
  .catch(error => {
    // Обробка помилки в разі неуспішного запиту
    console.log(error.message);
  });
```

```TypeScript
// Приклад відправлення POST-запиту з передачею даних
import axios from 'axios';

const requestBody = {
  name: 'John',
  age: 30
}

axios.post('https://api.example.com/users', requestBody)
  .then(response => {
    // Обробка отриманих даних
    console.log(response.data);
  })
  .catch(error => {
    // Обробка помилки в разі неуспішного запиту
    console.log(error.message);
  });
```

## Глибоке дослідження

Відправляючи HTTP-запити, необхідно врахувати деякі особливості. Наприклад, важливо використовувати правильний метод запиту (GET, POST, PUT, DELETE тощо) та передавати необхідні дані у відповідному форматі. Також необхідно обробляти помилки, які можуть виникнути під час відправлення запиту.

## Дивіться також

- [Методи HTTP-запитів](https://developer.mozilla.org/uk/docs/Web/HTTP/Methods)
- [Axios - бібліотека для роботи з HTTP-запитами](https://github.com/axios/axios)
- [Розробка веб-додатків з TypeScript](https://www.typescriptlang.org/docs/handbook/react.html)