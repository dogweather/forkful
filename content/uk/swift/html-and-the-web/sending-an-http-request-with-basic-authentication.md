---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/swift/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:46.083949-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request-with-basic-authentication.md"
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
