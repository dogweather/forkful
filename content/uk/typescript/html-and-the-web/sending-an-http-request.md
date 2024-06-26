---
date: 2024-01-20 18:01:21.878867-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u0440\u0438\u043C\u0456\u0442\u043A\u0430: \u0412\u0441\u0442\u0430\u043D\
  \u043E\u0432\u0456\u0442\u044C `axios` \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\
  \u043E\u0433\u043E\u044E `npm install axios`, \u043F\u0435\u0440\u0448 \u043D\u0456\
  \u0436 \u0437\u0430\u043F\u0443\u0441\u043A\u0430\u0442\u0438 \u043A\u043E\u0434\
  \ \u0432\u0438\u0449\u0435. \u041F\u0440\u0438\u043A\u043B\u0430\u0434 \u0432\u0438\
  \u0432\u0435\u0434\u0435\u043D\u043D\u044F."
lastmod: '2024-04-05T22:38:47.954843-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0440\u0438\u043C\u0456\u0442\u043A\u0430: \u0412\u0441\u0442\u0430\
  \u043D\u043E\u0432\u0456\u0442\u044C `axios` \u0437\u0430 \u0434\u043E\u043F\u043E\
  \u043C\u043E\u0433\u043E\u044E `npm install axios`, \u043F\u0435\u0440\u0448 \u043D\
  \u0456\u0436 \u0437\u0430\u043F\u0443\u0441\u043A\u0430\u0442\u0438 \u043A\u043E\
  \u0434 \u0432\u0438\u0449\u0435. \u041F\u0440\u0438\u043A\u043B\u0430\u0434 \u0432\
  \u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## Як це зробити:
```TypeScript
import axios from 'axios';

async function fetchUserData(userId: string) {
  try {
    const response = await axios.get(`https://api.example.com/users/${userId}`);
    console.log(response.data);
  } catch (error) {
    console.error(error);
  }
}

fetchUserData('12345');
```
Примітка: Встановіть `axios` за допомогою `npm install axios`, перш ніж запускати код вище. 

Приклад виведення:
```json
{
  "id": "12345",
  "name": "Oleksiy",
  "email": "oleksiy@example.com"
}
```

## Глибоке занурення:
Відправка HTTP-запитів - не новина, ця практика триває майже від початку вебу. `XMLHttpRequest` був золотим стандартом у JS, але його замінили сучасніші API, такі як `fetch`. TypeScript, надбудова над JavaScript, дозволяє використовувати ті самі інтерфейси з додатковими перевагами типізації.

`fetch` – це нативний спосіб відправляти запити, але у нас є бібліотеки, такі як `axios`, які пропонують додатковий функціонал та легшість у використанні. З `axios`, наприклад, легко обробляти помилки, використовувати проміси, і він має автоматичне перетворення JSON.

У TypeScript, при використовуванні `axios` чи `fetch`, важливо використовувати інтерфейси чи типи для відповідей, щоб забезпечити типізацію і підвищити якість коду.

## Дивись також:
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/uk/docs/Web/API/Fetch_API)
- [Axios GitHub repository](https://github.com/axios/axios)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
