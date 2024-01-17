---
title:                "Надсилання запиту http з базовою аутентифікацією"
html_title:           "TypeScript: Надсилання запиту http з базовою аутентифікацією"
simple_title:         "Надсилання запиту http з базовою аутентифікацією"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Що і для чого?

Відправка HTTP-запиту з базовою аутентифікацією - це процес, за допомогою якого програмісти обмінюються даними між клієнтом і сервером. Такий тип аутентифікації застосовується для захисту чутливих даних та контролю доступу до ресурсів.

## Як це зробити:

```TypeScript
// Встановлюємо бібліотеку для роботи з HTTP-запитами
import axios from 'axios';

// Виконуємо HTTP-запит з базовою аутентифікацією
axios.get('https://example.com', {
  auth: {
    username: 'username',
    password: 'password'
  }
})

// Отримуємо дані з запиту
.then(response => {
  console.log(response.data);
})

// Обробляємо помилки
.catch(error => {
  console.log(error);
});
```

## Глибоке погруження:

Цей метод аутентифікації став широко використовуваним у веб-розробці в перші роки Інтернету. Він використовувався для передачі незашифрованого користувача та пароля в HTTP-запитах. Зараз більш безпечними альтернативами є OAuth і JWT. Для виконання HTTP-запитів з базовою аутентифікацією потрібно налаштувати правильні дані в об'єкті "auth", який передається як опція до методу `http.get`.

## Додаткова інформація:

- [Офіційна документація Axios](https://axios-http.com/)
- [Офіційна документація TypeScript](https://www.typescriptlang.org/)
- [Стаття про базову аутентифікацію на MDN](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)