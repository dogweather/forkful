---
date: 2024-01-20 18:02:46.083949-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0411\u0430\u0437\u043E\u0432\u0430 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\
  \u0444\u0456\u043A\u0430\u0446\u0456\u044F \u0432 HTTP \u2014 \u0446\u0435 \u043C\
  \u0435\u0442\u043E\u0434 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\
  \u0430\u0446\u0456\u0457, \u0449\u043E \u0431\u0443\u0432 \u0432\u0432\u0435\u0434\
  \u0435\u043D\u0438\u0439 \u0449\u0435 \u0443 HTTP/1.0. \u0410\u043B\u0435 \u0437\
  \ \u043E\u0433\u043B\u044F\u0434\u0443 \u043D\u0430 \u0457\u0457 \u0441\u043B\u0430\
  \u0431\u043A\u0456 \u0441\u0442\u043E\u0440\u043E\u043D\u0438, \u043E\u0441\u043E\
  \u0431\u043B\u0438\u0432\u043E\u2026"
lastmod: '2024-04-05T21:53:49.991814-06:00'
model: gpt-4-1106-preview
summary: "\u0411\u0430\u0437\u043E\u0432\u0430 \u0430\u0443\u0442\u0435\u043D\u0442\
  \u0438\u0444\u0456\u043A\u0430\u0446\u0456\u044F \u0432 HTTP \u2014 \u0446\u0435\
  \ \u043C\u0435\u0442\u043E\u0434 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\
  \u0456\u043A\u0430\u0446\u0456\u0457, \u0449\u043E \u0431\u0443\u0432 \u0432\u0432\
  \u0435\u0434\u0435\u043D\u0438\u0439 \u0449\u0435 \u0443 HTTP/1.0."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
