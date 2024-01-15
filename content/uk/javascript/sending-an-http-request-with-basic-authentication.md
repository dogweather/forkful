---
title:                "Надсилання http-запиту з основною аутентифікацією"
html_title:           "Javascript: Надсилання http-запиту з основною аутентифікацією"
simple_title:         "Надсилання http-запиту з основною аутентифікацією"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Посилання HTTP з базовою аутентифікацією - це широко використовуваний спосіб забезпечення безпеки веб-додатків. Використання базової аутентифікації дозволяє передавати дані для аутентифікації в зашифрованому форматі, що дозволяє забезпечити захист від несанкціонованого доступу до важливих даних.

## Як це зробити

```Javascript
// Приклад використання fetch API для відправки запиту HTTP з базовою аутентифікацією

fetch('https://example.com/api/', {
  method: 'GET',
  headers: {
    Authorization: 'Basic ' + btoa(username + ':' + password)
    // Використання функції btoa для кодування логіна та пароля в форматі base64
  }
})
.then(res => res.json())
.then(data => {
  console.log(data); // Виведення відповіді сервера у консоль
})
.catch(error => {
  console.log(error); // Виведення помилки, якщо запит не був успішно виконаний
});
```

### Вихідні дані:
```
{"message": "Дані успішно отримані!"}
```

## Глибокий аналіз

Перед тим як відправити запит з базовою аутентифікацією, необхідно знати логін та пароль для доступу до застосунку. Також, важливо перевірити, чи сервер підтримує базову аутентифікацію, вказавши правильне значення для заголовка `Authorization`. Для забезпечення безпеки, рекомендується використовувати HTTPS замість HTTP при виконанні запиту з базовою аутентифікацією.

## Дивись також

- [MDN Web Docs: Fetch API](https://developer.mozilla.org/uk/docs/Web/API/Fetch_API)
- [MDN Web Docs: base64](https://developer.mozilla.org/uk/docs/Web/API/WindowOrWorkerGlobalScope/btoa)