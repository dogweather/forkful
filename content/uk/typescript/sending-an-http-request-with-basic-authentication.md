---
title:                "TypeScript: Відправлення http-запиту з основною аутентифікацією"
simple_title:         "Відправлення http-запиту з основною аутентифікацією"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP запиту з базовою аутентифікацією - це важлива частина розробки веб-додатків, особливо коли мова йде про захист конфіденційної інформації. Це є надійним способом забезпечити, що тільки користувачі з вірними обліковими даними отримують доступ до захищених ресурсів.

## Як

Існує кілька способів відправки HTTP запиту з базовою аутентифікацією в TypeScript. Один з них - використання бібліотеки "axios", яка надає зручний інтерфейс для створення і відправлення запитів. Давайте розглянемо приклад коду, який показує, як використовувати "axios" для відправки запиту з базовою аутентифікацією:

```TypeScript
import axios from 'axios';

axios.get('https://example.com/protected-resource', {
  auth: {
    username: 'username',
    password: 'password'
  }
})
  .then(response => {
    console.log(response.data); // виводить дані захищеного ресурсу
  })
  .catch(error => {
    console.log(error);
  });
```

В цьому прикладі ми використовуємо метод "get" для відправки GET запиту та передаємо об'єкт з параметром "auth", який містить облікові дані користувача. Після успішної відправки запиту, результат знаходиться в властивості "data" об'єкта "response". В разі помилки, вона буде виведена в консолі.

## Глибокий розгляд

Необов'язково використовувати бібліотеку "axios" для надсилання HTTP запитів з базовою аутентифікацією. Також можна вручну створити запит з використанням класу "XMLHttpRequest" у TypeScript. Нижче наведено приклад коду, який показує як це зробити:

```TypeScript
let xhr = new XMLHttpRequest();
xhr.open('GET', 'https://example.com/protected-resource');
xhr.setRequestHeader('Authorization', 'Basic ' + btoa('username:password')); // конвертуємо об'єкт з обліковими даними в base64 рядок
xhr.onload = function() {
  console.log(xhr.responseText); // виводить дані захищеного ресурсу
};
xhr.send();
```

В цьому прикладі ми відкриваємо запит методом "open", встановлюємо заголовок авторизації з обліковими даними та вказуємо, що відповідь повинна бути передана у властивість "responseText" об'єкта "xhr". За допомогою методу "send" ми відправляємо запит і отримуємо дані захищеного ресурсу.

## Дивись також

- [Документація бібліотеки Axios](https://axios-http.com/docs/intro)