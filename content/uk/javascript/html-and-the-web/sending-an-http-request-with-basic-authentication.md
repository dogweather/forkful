---
date: 2024-01-20 18:02:12.232933-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0411\u0430\u0437\u043E\u0432\u0430 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\
  \u0444\u0456\u043A\u0430\u0446\u0456\u044F \u2014 \u0441\u0442\u0430\u0440\u0438\
  \u0439, \u0430\u043B\u0435 \u043F\u0440\u043E\u0441\u0442\u0438\u0439 \u0441\u043F\
  \u043E\u0441\u0456\u0431 \u0437\u0430\u0445\u0438\u0441\u0442\u0443. \u0412\u0438\
  \u043D\u0438\u043A\u043B\u0430 \u043D\u0430 \u043F\u043E\u0447\u0430\u0442\u043A\
  \u0443 \u0432\u0435\u0431\u0443, \u0432 HTTP/1.0. \u0417\u0430\u0431\u0435\u0437\
  \u043F\u0435\u0447\u0443\u0454 \u043C\u0456\u043D\u0456\u043C\u0430\u043B\u044C\u043D\
  \u0443 \u0431\u0435\u0437\u043F\u0435\u043A\u0443, \u043E\u0441\u043A\u0456\u043B\
  \u044C\u043A\u0438\u2026"
lastmod: '2024-04-05T22:51:02.898219-06:00'
model: gpt-4-1106-preview
summary: "\u0411\u0430\u0437\u043E\u0432\u0430 \u0430\u0443\u0442\u0435\u043D\u0442\
  \u0438\u0444\u0456\u043A\u0430\u0446\u0456\u044F \u2014 \u0441\u0442\u0430\u0440\
  \u0438\u0439, \u0430\u043B\u0435 \u043F\u0440\u043E\u0441\u0442\u0438\u0439 \u0441\
  \u043F\u043E\u0441\u0456\u0431 \u0437\u0430\u0445\u0438\u0441\u0442\u0443."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
