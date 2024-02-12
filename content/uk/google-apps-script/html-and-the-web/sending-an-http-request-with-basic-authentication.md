---
title:                "Відправлення HTTP-запиту з базовою аутентифікацією"
date:                  2024-02-01T22:02:42.420883-07:00
model:                 gpt-4-0125-preview
simple_title:         "Відправлення HTTP-запиту з базовою аутентифікацією"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Відправлення HTTP-запиту з базовою аутентифікацією передбачає кодування імені користувача та пароля у заголовок запиту для доступу до захищених ресурсів. Програмісти використовують цей метод для аутентифікації на серверній стороні, щоб інтегруватися з API, які вимагають базової автентифікації для операцій, таких як отримання даних або розміщення контенту.

## Як це зробити:

У Google Apps Script, для відправки HTTP-запиту з базовою аутентифікацією, ви використовуєте сервіс `UrlFetchApp` разом з заголовком авторизації, закодованим у base64. Ось покроковий посібник:

1. **Кодування облікових даних**: Перш за все, закодуйте ваше ім'я користувача та пароль у base64. Google Apps Script не має нативної функції кодування рядків у base64, тому для цього використовуйте Utilities.base64Encode.

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Налаштування параметрів запиту**: За допомогою готових закодованих облікових даних, підготуйте об'єкт параметрів для HTTP-запиту, включаючи метод і заголовки.

```javascript
var options = {
  method: 'get', // або 'post', 'put', залежно від ваших потреб
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // додаткові параметри, як-от 'muteHttpExceptions' для обробки помилок, можуть бути додані тут
};
```

3. **Відправлення запиту**: Використовуйте метод `UrlFetchApp.fetch` з цільовим URL і об'єктом параметрів.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

Приклад виводу при успішному запиті буде змінюватися в залежності від відповіді API. Для API, який базується на JSON, ви можете побачити щось на кшталт:

```
{"status":"Success","data":"Resource data here..."}
```

Переконайтеся, що ви обробляєте можливі HTTP помилки, перевіряючи код відповіді або використовуючи параметр `muteHttpExceptions` для більш контрольованого управління помилками.

## Поглиблене вивчення

Відправлення HTTP-запиту з базовою аутентифікацією було стандартним методом у багатьох мовах програмування для доступу до веб-ресурсів, які вимагають аутентифікації. У контексті Google Apps Script, `UrlFetchApp` пропонує прямолінійний спосіб виконувати ці HTTP-запити, включаючи ті, що вимагають аутентифікації. Включення базових облікових даних у заголовки запиту є простим, але ефективним методом, але воно має певні недоліки з точки зору безпеки, перш за все тому, що облікові дані передаються у відкритому тексті, просто закодовані у base64, що може бути легко декодовано, якщо їх перехопити.

Для покращення безпеки рекомендуються альтернативи, такі як OAuth 2.0, особливо при роботі з чутливими даними або операціями. Google Apps Script має вбудовану підтримку OAuth 2.0 із бібліотекою `OAuth2`, що спрощує процес аутентифікації проти сервісів, які підтримують цей протокол.

Незважаючи на свої обмеження з точки зору безпеки, базова аутентифікація продовжує широко використовуватися для простих або внутрішніх застосунків, що не виставляються у широкий доступ в інтернеті. Її легко реалізувати, оскільки вона вимагає лише одного запиту з належно налаштованими заголовками, роблячи її привабливим варіантом для швидких інтеграцій або для API, де методи вищої безпеки не доступні. Однак програмістам настійно рекомендується враховувати вплив на безпеку та розглядати безпечніші альтернативи, коли вони доступні.