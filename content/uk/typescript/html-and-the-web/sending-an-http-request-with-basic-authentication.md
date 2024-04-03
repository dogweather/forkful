---
date: 2024-01-20 18:02:53.946575-07:00
description: "\u0429\u043E \u0456 \u0447\u043E\u043C\u0443? \u0412\u0456\u0434\u043F\
  \u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0438\u0442\u0443 \u0437\
  \ \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\u0432\u0442\u0435\u043D\u0442\
  \u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E \u2013 \u0446\u0435 \u0441\
  \u043F\u043E\u0441\u0456\u0431 \u043F\u0435\u0440\u0435\u0434\u0430\u0447\u0456\
  \ \u043B\u043E\u0433\u0456\u043D\u0430 \u0456 \u043F\u0430\u0440\u043E\u043B\u044F\
  \ \u0434\u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\u0443 \u0434\u043E \u0440\
  \u0435\u0441\u0443\u0440\u0441\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:48.867669-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2013 \u0446\u0435 \u0441\u043F\u043E\u0441\u0456\u0431 \u043F\u0435\u0440\u0435\
  \u0434\u0430\u0447\u0456 \u043B\u043E\u0433\u0456\u043D\u0430 \u0456 \u043F\u0430\
  \u0440\u043E\u043B\u044F \u0434\u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\
  \u0443 \u0434\u043E \u0440\u0435\u0441\u0443\u0440\u0441\u0443."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## Що і чому?
Відправка HTTP-запиту з базовою автентифікацією – це спосіб передачі логіна і пароля для доступу до ресурсу. Програмісти використовують це для безпечної взаємодії з захищеними API.

## How to:


## Як це зробити:
```TypeScript
import axios from 'axios';

const getProtectedData = async () => {
  try {
    const response = await axios.get('http://example.com/data', {
      auth: {
        username: 'yourUsername',
        password: 'yourPassword'
      }
    });
    console.log(response.data);
  } catch (error) {
    console.error('There was an error!', error);
  }
};

getProtectedData();
```

Sample output:
```json
{
  "protected": "data"
}
```

## Deep Dive


## Поглиблений аналіз
Базова автентифікація, це стандартний механізм в HTTP, який був введений ще в HTTP/1.0. Незважаючи на вік, досі застосовується через простоту реалізації. Існують альтернативи, наприклад, OAuth та JWT, які забезпечують більш безпечну та гнучку автентифікацію. Ключова особливість базової автентифікації – закодування логіна та пароля в Base64, але важливо пам'ятати, що без захищеного з'єднання (HTTPS) ці дані можна легко перехопити.

## See Also


## Дивіться також
- MDN Web Docs on HTTP authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Axios documentation: https://github.com/axios/axios
- Understanding Base64 encoding: https://www.base64encode.org/
