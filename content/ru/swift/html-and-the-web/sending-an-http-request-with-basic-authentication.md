---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:57.746740-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u043E\u0442\u043F\u0440\u0430\
  \u0432\u0438\u0442\u044C HTTP-\u0437\u0430\u043F\u0440\u043E\u0441 \u0441 \u0431\
  \u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\
  \u0438\u043A\u0430\u0446\u0438\u0435\u0439 \u0432 Swift."
lastmod: '2024-03-13T22:44:45.683079-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u043E\u0442\u043F\u0440\u0430\u0432\
  \u0438\u0442\u044C HTTP-\u0437\u0430\u043F\u0440\u043E\u0441 \u0441 \u0431\u0430\
  \u0437\u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\
  \u043A\u0430\u0446\u0438\u0435\u0439 \u0432 Swift."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
Вот как отправить HTTP-запрос с базовой аутентификацией в Swift:

```Swift
import Foundation

// Ваш API endpoint
let url = URL(string: "https://example.com/api/data")!

// Ваши учетные данные
let username = "user"
let password = "password"

// Создаем данные для входа и конвертируем их в строку base64
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

// Создаем запрос
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

// Отправляем запрос
let session = URLSession.shared
let dataTask = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Ошибка: \(error)") // Обработка ошибки
    } else if let data = data, let string = String(data: data, encoding: .utf8) {
        print("Ответ: \(string)") // Обработка ответа
    }
}

dataTask.resume()
```

Вывод должен быть данными, возвращенными из API, или сообщением об ошибке, если что-то пошло не так.

## Погружение в подробности
В начале эры интернета базовая аутентификация была быстрым способом защитить ресурсы. Ее простота способствовала широкому распространению, несмотря на то, что она менее безопасна, чем современные альтернативы типа OAuth, потому что учетные данные не шифруются, а кодируются.

Помимо базовой аутентификации, существуют альтернативы, такие как аутентификация digest, API ключи, OAuth или JWT (JSON Web Tokens). Каждая из них имеет свои плюсы и минусы с точки зрения безопасности, удобства использования и уровня предоставляемой защиты.

При отправке HTTP-запроса с базовой аутентификацией лучше всего убедиться, что вы используете HTTPS, чтобы ваши закодированные учетные данные передавались безопасно. Также избегайте жесткого кодирования учетных данных; вместо этого используйте переменные среды или безопасные хранилища.

## См. также
- [URLSession от Apple](https://developer.apple.com/documentation/foundation/urlsession)
- [RFC базовой аутентификации HTTP](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
