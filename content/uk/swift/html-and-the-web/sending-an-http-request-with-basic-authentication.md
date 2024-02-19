---
aliases:
- /uk/swift/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:46.083949-07:00
description: "HTTP-\u0437\u0430\u043F\u0438\u0442\u0438 \u0437 \u0431\u0430\u0437\u043E\
  \u0432\u043E\u044E \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\
  \u0446\u0456\u0454\u044E \u2014 \u0446\u0435 \u0442\u0435\u0445\u043D\u0456\u043A\
  \u0430 \u0437\u0430\u0445\u0438\u0441\u0442\u0443, \u044F\u043A\u0430 \u0432\u0438\
  \u043C\u0430\u0433\u0430\u0454 \u0432\u0456\u0434 \u043A\u043E\u0440\u0438\u0441\
  \u0442\u0443\u0432\u0430\u0447\u0430 \u0432\u0432\u0435\u0441\u0442\u0438 \u043B\
  \u043E\u0433\u0456\u043D \u0456 \u043F\u0430\u0440\u043E\u043B\u044C, \u0449\u043E\
  \u0431 \u043E\u0442\u0440\u0438\u043C\u0430\u0442\u0438 \u0434\u043E\u0441\u0442\
  \u0443\u043F \u0434\u043E \u0440\u0435\u0441\u0443\u0440\u0441\u0443. \u0420\u043E\
  \u0437\u0440\u043E\u0431\u043D\u0438\u043A\u0438\u2026"
lastmod: 2024-02-18 23:09:00.960587
model: gpt-4-1106-preview
summary: "HTTP-\u0437\u0430\u043F\u0438\u0442\u0438 \u0437 \u0431\u0430\u0437\u043E\
  \u0432\u043E\u044E \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\
  \u0446\u0456\u0454\u044E \u2014 \u0446\u0435 \u0442\u0435\u0445\u043D\u0456\u043A\
  \u0430 \u0437\u0430\u0445\u0438\u0441\u0442\u0443, \u044F\u043A\u0430 \u0432\u0438\
  \u043C\u0430\u0433\u0430\u0454 \u0432\u0456\u0434 \u043A\u043E\u0440\u0438\u0441\
  \u0442\u0443\u0432\u0430\u0447\u0430 \u0432\u0432\u0435\u0441\u0442\u0438 \u043B\
  \u043E\u0433\u0456\u043D \u0456 \u043F\u0430\u0440\u043E\u043B\u044C, \u0449\u043E\
  \u0431 \u043E\u0442\u0440\u0438\u043C\u0430\u0442\u0438 \u0434\u043E\u0441\u0442\
  \u0443\u043F \u0434\u043E \u0440\u0435\u0441\u0443\u0440\u0441\u0443. \u0420\u043E\
  \u0437\u0440\u043E\u0431\u043D\u0438\u043A\u0438\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
---

{{< edit_this_page >}}

## Що це та навіщо?
HTTP-запити з базовою аутентифікацією — це техніка захисту, яка вимагає від користувача ввести логін і пароль, щоб отримати доступ до ресурсу. Розробники використовують це, щоб забезпечити контроль доступу до серверних ресурсів.

## Як це зробити:
```swift
import Foundation

// Встановлення URL та вашого логіна/пароля
let url = URL(string: "https://yourapiendpoint.com/data")!
let login = "your_login"
let password = "your_password"

// Підготовка закодованого рядка авторизації
let loginString = "\(login):\(password)"
guard let loginData = loginString.data(using: .utf8) else {
    fatalError("Unable to encode login data")
}
let base64LoginString = loginData.base64EncodedString()

// Створення запиту
var request = URLRequest(url: url)
request.httpMethod = "GET" // або "POST", "PUT", "DELETE" в залежності від потреби
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Відправка запиту
let session = URLSession.shared
let task = session.dataTask(with: request) { data, response, error in
    guard let httpResponse = response as? HTTPURLResponse,
          (200...299).contains(httpResponse.statusCode) else {
        print("Authorization failed")
        return
    }
    if let data = data {
        // Тут ваш код для обробки отриманих даних
        print("Data received")
    }
}
task.resume()
```

## Поглиблений розгляд
Базова аутентифікація в HTTP — це метод аутентифікації, що був введений ще у HTTP/1.0. Але з огляду на її слабкі сторони, особливо небезпеку перехоплення логіна та пароля, зараз часто використовують більш безпечні методи, як OAuth. Тим не менш, для закритих мереж або систем, де високий рівень безпеки не є критичним, базова аутентифікація все ще популярна через простоту використання. Перед відправкою логіна та пароля вони кодуються у base64, але зверніть увагу, що це не є надійним шифруванням, і такі дані можуть бути легко розкодовані. Рекомендується використовувати HTTPS, щоб захистити дані під час передачі.

## Додаткові ресурси
- [Apple Developer - URL Loading System](https://developer.apple.com/documentation/foundation/url_loading_system)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [OWASP - Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)
