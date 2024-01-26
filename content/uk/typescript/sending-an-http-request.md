---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T18:01:21.878867-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?
Відправка HTTP-запиту - це спосіб, яким ваша програма запитує дані або надсилає інформацію до іншого сервера через інтернет. Програмісти роблять це, щоб обмінюватися даними між різними сервісами та реалізовувати мережеві взаємодії в своїх додатках.

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
