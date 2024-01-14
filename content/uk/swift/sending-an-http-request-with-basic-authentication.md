---
title:                "Swift: Надсилання запиту http з основною аутентифікацією"
simple_title:         "Надсилання запиту http з основною аутентифікацією"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Відправлення HTTP-запиту з базовою автентифікацією є важливим кроком у забезпеченні безпечної та захищеної комунікації між сервером та клієнтом.

## Як

За допомогою базової автентифікації, клієнт може надіслати запит на сервер, включаючи інформацію про свій ідентифікатор та пароль. Це дозволяє серверу перевірити ідентичність клієнта та надійно засекретити дані.

```Swift
// Створення запиту з базовою автентифікацією
var url = URL(string: "https://www.example.com")!
var request = URLRequest(url: url)
request.addValue("Basic YWxhZGRpbjpvcGVuc2VzYW1l", forHTTPHeaderField: "Authorization")

// Відправлення запиту та обробка відповіді
let session = URLSession.shared
session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Сталася помилка: \(error.localizedDescription)")
    }
    guard let data = data else { return }
    print("Отримана відповідь: \(String(data: data, encoding: .utf8) ?? "")")
}.resume()
```

### Вихід

Запит: `https://www.example.com`
Метод: `GET`
Заголовки:
```
Authorization: Basic YWxhZGRpbjpvcGVuc2VzYW1l
```

## Deep Dive

Автентифікація в рамках HTTP використовує заголовок `Authorization`, який містить тип автентифікації та дані користувача у форматі `username:password`. Перед надсиланням запиту, користувачі повинні закодувати цю інформацію у форматі base64 та включити її у заголовок `Authorization`.

Базова автентифікація не є надійним методом безпеки, оскільки дані кодуються лише у форматі base64, який може бути легко декодований. Тому варто розглянути більш безпечні методи автентифікації, наприклад, HTTPS.

## Дивись також

- [HTTP Authentication: Basic and Digest Access Authentication](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)
- [Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)