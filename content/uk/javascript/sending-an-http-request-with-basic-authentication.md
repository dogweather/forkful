---
title:                "Надсилання http-запиту з основною автентифікацією"
html_title:           "Javascript: Надсилання http-запиту з основною автентифікацією"
simple_title:         "Надсилання http-запиту з основною автентифікацією"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?
Надсилання HTTP-запиту з базовою аутентифікацією - це процес передачі запиту до веб-сервера з додатковими обліковими даними для перевірки ідентифікації користувача. Це може бути необхідно для доступу до певних обмежених ресурсів та забезпечення безпеки.

## Як це зробити:
```Javascript
fetch('https://example.com/api', {
    headers: {
        Authorization: 'Basic ' + btoa('username:password')
    }
})
    .then(response => console.log(response))
    .catch(error => console.log(error))
```

## Глибока пірнятка:
- Історичний контекст: базова аутентифікація HTTP була першою формою аутентифікації для HTTP-протоколу.
- Альтернативи: існує багато інших типів аутентифікації для HTTP, таких як OAuth і JWT.
- Деталі реалізації: для передачі облікових даних у запиті можна використовувати шифрування Base64.

## Дивіться також:
- [MDN - Basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [HTTP Authentication: Basic and Digest Access Authentication (RFC-7617)](https://tools.ietf.org/html/rfc7617)