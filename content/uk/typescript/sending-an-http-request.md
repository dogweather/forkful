---
title:                "TypeScript: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Для чого

Надсилання HTTP запитів є необхідною частиною програмування на TypeScript. Це дозволяє отримати дані з різних джерел, таких як віддалений сервер або API, у вашій програмі.

## Як це зробити

Для надсилання HTTP запиту використовується вбудований клас `HttpClient`. Нижче подані приклади коду і вихідні дані для різних типів запитів.

```TypeScript
// GET запит
import { HttpClient } from '@angular/common/http';

const URL = 'https://example.com/users';
const httpClient = new HttpClient();
httpClient.get(URL).subscribe(data => {
  console.log(data); // виводить отримані дані у форматі JSON
});
```

```
Output:
{ "id": 1, "name": "John", "email": "john@example.com" }
```

```TypeScript
// POST запит з тілом
import { HttpClient } from '@angular/common/http';

const URL = 'https://example.com/users';
const httpClient = new HttpClient();
const body = { name: 'Kate', email: 'kate@example.com' };
httpClient.post(URL, body).subscribe(data => {
  console.log(data); // виводить отриману відповідь сервера у форматі JSON
});
```

```
Output:
{ "success": true, "message": "User added successfully" }
```

```TypeScript
// PUT запит з параметрами
import { HttpClient, HttpParams } from '@angular/common/http';

const URL = 'https://example.com/users';
const httpClient = new HttpClient();
const params = new HttpParams().set('id', '2');
httpClient.put(URL, {}, { params }).subscribe(data => {
  console.log(data); // виводить отриману відповідь сервера у форматі JSON
});
```

```
Output:
{ "success": true, "message": "User updated successfully" }
```

```TypeScript
// DELETE запит з заголовком
import { HttpClient, HttpHeaders } from '@angular/common/http';

const URL = 'https://example.com/users';
const httpClient = new HttpClient();
const headers = new HttpHeaders().set('Authorization', 'Bearer xxxxxx');
httpClient.delete(URL, { headers }).subscribe(data => {
  console.log(data); // виводить отриману відповідь сервера у форматі JSON
});
```

```
Output:
{ "success": true, "message": "User deleted successfully" }
```

## Детальний розбір

Для початку необхідно імпортувати `HttpClient` та інші необхідні класи з `@angular/common/http`. Потім можна створити екземпляр `HttpClient`, вказавши URL, на який потрібно надіслати запит. Далі, використовуючи методи `get()`, `post()`, `put()` і `delete()`, можна відправити запит на сервер, передаючи необхідні параметри та заголовки. У підписці на `subscribe()` можна отримати дані з сервера та обробити їх.

## Дивіться також

- [Angular документація про HttpClient](https://angular.io/guide/http)
- [Стаття про надсилання HTTP запитів в TypeScript](https://www.digitalocean.com/community/tutorials/angular-angular-httpclient)
- [Програмування на TypeScript: підручник](https://prog.kiev.ua/uchebnik-po-terminologii-js/ts/typescript/)