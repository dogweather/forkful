---
title:                "Відправлення HTTP-запиту"
html_title:           "Bash: Відправлення HTTP-запиту"
simple_title:         "Відправлення HTTP-запиту"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що це & для чого це потрібно?

Відправка HTTP-запиту - це процес, коли ваш код (або додаток) запитує інформацію від сервера через Інтернет. Програмісти це роблять, щоб отримувати, відправляти, оновлювати або видаляти дані з віддалених веб-серверів.

## Як зробити:

```Swift
import Foundation

let url = URL(string: "https://example.com")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let data = data {
        let str = String(data: data, encoding: .utf8)
        print(str ?? "")
    }
}
task.resume()
```

вихід:

```Swift
{
    "exampleKey": "exampleValue"
}
```

## Детальніше:

1. **Історичний контекст**. HTTP-запити використовуються з моменту створення Інтернету. Swift вводить простоту відправки цих запитів через URLSession.
2. **Альтернативи**. Можна також використовувати бібліотеки третіх сторін, такі як Alamofire, для спрощення процесу.
3. **Застосування**. HTTP-запити - це основа мережевих операцій у вашому додатку. URLSession використовує сессію, що має налаштування, які обмежують життєвий цикл мережевого запиту.

## Див. також:

1. "URLSession" - Офіційна документація Apple: [посилання](https://developer.apple.com/documentation/foundation/urlsession)
2. "Alamofire" - Бібліотека HTTP Networking для Swift: [посилання](https://github.com/Alamofire/Alamofire)
4. "How to send HTTP requests using URLSession" - стаття від Hacking with Swift: [посилання](https://www.hackingwithswift.com/example-code/networking/how-to-send-http-requests-using-urlsession)