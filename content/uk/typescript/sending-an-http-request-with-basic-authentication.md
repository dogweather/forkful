---
title:                "Відправлення запиту http з базовою аутентифікацією."
html_title:           "TypeScript: Відправлення запиту http з базовою аутентифікацією."
simple_title:         "Відправлення запиту http з базовою аутентифікацією."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Чому

За допомогою HTTP-запиту з базовою аутентифікацією можна надіслати запит до веб-сервера і отримати доступ до захищених ресурсів. Це може бути корисно для створення безпеки в додатках чи взаємодії зі сторонніми сервісами.

## Як це зробити

```TypeScript
// Імпортуємо необхідні бібліотеки
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { map } from 'rxjs/operators';

// Встановлюємо необхідні опції для запиту (авторизаційні дані)
const username = 'username';
const password = 'password';
const options = {
  headers: new HttpHeaders({
    'Authorization': 'Basic ' + btoa(username + ':' + password) // конвертація в Base64
  })
};

// Cтворюємо екземпляр HttpClient і виконуємо GET-запит з опціями
this.http.get('https://example.com/api/resource', options).pipe(
  map((response: any) => {
    // Обробка отриманих даних
    console.log(response);
  })
).subscribe();
```

Вищезазначений код показує, як зробити GET-запит з базовою аутентифікацією до веб-сервера за допомогою Angular HttpClient. Ми встановлюємо авторизаційні дані в заголовок запиту і отримуємо доступ до захищених ресурсів. Такий же підхід можна застосувати і для інших типів запитів (POST, PUT, DELETE).

## Глибше дослідження

HTTP-запит з базовою аутентифікацією полягає в тому, що ми додаємо заголовок `Authorization` зі значенням `Basic ` та закодованими авторизаційними даними в форматі Base64. Веб-сервер перевіряє цей заголовок та дозволяє доступ до захищених ресурсів, якщо дані вказані вірно.

При створенні додатків або роботі зі сторонніми сервісами, можуть виникати ситуації, коли необхідно забезпечити безпеку та захист ваших даних. Використання базової аутентифікації може допомогти в цьому, дозволяючи доступ до ресурсів лише після успішної авторизації.

## Дивіться також

- [Angular: HttpClient](https://angular.io/guide/http)
- [MDN: HTTP аутентифікація](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)