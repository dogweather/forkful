---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Завантаження веб-сторінки - це процес отримання веб-контенту з сервера до локальної машини. Програмісти цим займаються для обробки вмісту веб-сторінки в програмі (для скрапінгу, аналітики тощо).

## Як це зробити:

Можна використовувати URLSession, вбудовану в Swift. Погляньте на приклад:

```swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let data = data {
        print(String(data: data, encoding: .utf8) ?? "")
    }
}
task.resume()
```
Такий код буде завантажувати та виводити HTML веб-сторінки example.com.

## Поглиблюємося:

Рано чи пізно кожен розробник стикається з завантаженням веб-сторінок. URLSession від Swift це одна із можливостей. До нього могли б використовувати NSURLConnection, але він уже застарів. URLSession підтримує http, https, із продовженнями завантажень та інше.

Але є й альтернативи: Alamofire, бібліотека на Swift, що надає набагато більше можливостей, ніж URLSession (приклади: обробка JSON, перевірка стану мережі).

## Див. також:

- Офіційна документація URLSession від Apple: [тут](https://developer.apple.com/documentation/foundation/urlsession)
- Alamofire на GitHub: [тут](https://github.com/Alamofire/Alamofire)
- Вибір між URLSession та Alamofire: [тут](https://www.raywenderlich.com/6587213-urlsession-vs-alamofire-which-should-you-use)