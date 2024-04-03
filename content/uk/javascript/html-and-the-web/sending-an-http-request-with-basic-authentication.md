---
date: 2024-01-20 18:02:12.232933-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2014 \u0446\u0435 \u0441\u043F\u043E\u0441\u0456\u0431 \u043F\u0435\u0440\u0435\
  \u0434\u0430\u0447\u0456 \u043B\u043E\u0433\u0456\u043D\u0430 \u0456 \u043F\u0430\
  \u0440\u043E\u043B\u044F \u043D\u0430 \u0441\u0435\u0440\u0432\u0435\u0440 \u0434\
  \u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\u0443 \u0434\u043E \u0440\u0435\
  \u0441\u0443\u0440\u0441\u0456\u0432. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u043D\u0443\u044E\u0442\u044C\
  \ \u0446\u0435 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.992907-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2014 \u0446\u0435 \u0441\u043F\u043E\u0441\u0456\u0431 \u043F\u0435\u0440\u0435\
  \u0434\u0430\u0447\u0456 \u043B\u043E\u0433\u0456\u043D\u0430 \u0456 \u043F\u0430\
  \u0440\u043E\u043B\u044F \u043D\u0430 \u0441\u0435\u0440\u0432\u0435\u0440 \u0434\
  \u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\u0443 \u0434\u043E \u0440\u0435\
  \u0441\u0443\u0440\u0441\u0456\u0432."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
