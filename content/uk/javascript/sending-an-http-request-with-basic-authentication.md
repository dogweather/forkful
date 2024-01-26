---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:02:12.232933-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Відправка HTTP-запиту з базовою аутентифікацією — це спосіб передачі логіна і пароля на сервер для доступу до ресурсів. Програмісти виконують це для безпечного з'єднання з веб-сервісами, що вимагають авторизації.

## Як це зробити:

```Javascript
// Залучаємо вбудований модуль для відправки HTTP-запитів
const https = require('https');

// Додаємо логін та пароль
const auth = 'myUsername:myPassword';

// Кодуємо в base64
const encodedAuth = Buffer.from(auth).toString('base64');

// Задаємо параметри запиту
const options = {
  hostname: 'example.com',
  port: 443,
  path: '/api/data',
  method: 'GET',
  headers: {
    'Authorization': `Basic ${encodedAuth}`
  }
};

// Виконуємо запит
const req = https.request(options, (res) => {
  let responseBody = '';
  
  res.on('data', (chunk) => {
    responseBody += chunk;
  });

  res.on('end', () => {
    console.log('Отриманий відповідь: ', responseBody);
  });
});

req.on('error', (e) => {
  console.error('Проблема з запитом: ', e.message);
});

req.end();
```
Приклад виводу:
```
Отриманий відповідь: {"some":"data"}
```

## Глибше занурення

Базова аутентифікація — старий, але простий спосіб захисту. Виникла на початку вебу, в HTTP/1.0. Забезпечує мінімальну безпеку, оскільки логін і пароль кодуються, але не шифруються. Тому, важливо використовувати HTTPS.

Сьогодні є безпечніші альтернативи: OAuth, API ключі, токени. Проте, базова аутентифікація все ще корисна для простих випадків або внутрішніх мереж.

Деталі реалізації: логін і пароль передаються через заголовок `Authorization` у запиті. Кодування в base64 дозволяє передавати текст, що містить символи не з ASCII.

## Дивись також

- [MDN Web Docs - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Node.js https documentation](https://nodejs.org/api/https.html)
- [Buffer Node.js documentation](https://nodejs.org/api/buffer.html)
