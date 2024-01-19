---
title:                "Надсилання http-запиту з базовою аутентифікацією"
html_title:           "Arduino: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та чому?
Надсилання HTTP-запиту з базовою аутентифікацією - це процес, де користувач ідентифікує себе перед сервером за допомогою логіна та пароля. Програмісти роблять це, щоб забезпечити доступ до захищених ресурсів.

## Як це зробити:
```Swift
import Foundation

let username = "yourUsername"
let password = "yourPassword"
let loginString = String(format: "%@:%@", username, password)
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

let url = URL(string: "https://your-api.com")!
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```
У відповідь ви отримаєте дані від серверу або повідомлення про помилку, якщо щось пішло не так.

## Поглиблений розбір
1. Історичний контекст: Базова аутентифікація HTTP - це стандартний метод для передачі вхідних даних для аутентифікації. Єдність цього методу полягає в його широкій сумісності з різними системами.

2. Альтернативи: Несмотря на простоту базовой аутентифікації HTTP, є інші, більш безпечні варіанти, такі як OAuth та JWT. Вони зазвичай використовуються для веб-додатків з вищим рівнем безпеки.

3. Деталі реалізації: В Swift, для надсилання HTTP-запиту з базовою аутентифікацією, використовується `URLRequest` і `URLSession`. `URLRequest` формує ваш запит, а `URLSession` виконує його.

## Див. також
1. [URLSession - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
2. [URLRequest - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlrequest)
3. [OAuth - Official Website](https://oauth.net)
4. [JWT - Official Website](https://jwt.io)