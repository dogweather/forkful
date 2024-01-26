---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:02:53.946575-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
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
