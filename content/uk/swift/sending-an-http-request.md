---
title:                "Відправка http запиту"
html_title:           "Swift: Відправка http запиту"
simple_title:         "Відправка http запиту"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Існує багато причин, чому люди використовують HTTP запити у своїх програмах. Наприклад, щоб взаємодіяти зі сторонніми веб-серверами та отримувати дані, які можна використовувати в своїй програмі.

## Як

Щоб надіслати HTTP запит у Swift, спочатку потрібно ініціалізувати об'єкт `URLRequest` з необхідною URL адресою та методом запиту. Наприклад, якщо ми хочемо отримати дані з веб-сервера, то використовуємо метод `GET`:

```
let url = URL(string: "https://example.com/users")!
var request = URLRequest(url: url)
request.httpMethod = "GET"
```

Далі потрібно створити об'єкт `URLSession`, який буде відправляти наш запит та обробляти відповідь. Для цього використовується метод `dataTask`, якому передається наш запит та замикання, яке виконається після отримання відповіді:

```
let session = URLSession.shared
let task = session.dataTask(with: request) { data, response, error in
    // Обробка відповіді
}
```

Після того, як ми отримали дані, потрібно їх обробити. Наприклад, використовуючи метод `JSONSerialization` можна перетворити отриманий JSON у Swift об'єкт:

```
if let data = data {
    do {
        let jsonObject = try JSONSerialization.jsonObject(with: data, options: [])
        // Використовувати отримані дані
    } catch {
        // Обробка помилки
    }
}
```

## Deep Dive

Під час відправки HTTP запиту, ми можемо вказати інші параметри, такі як заголовки, тіло запиту чи таймаут. Також, в рамках блоку замикання `dataTask` ми можемо виконати будь-які додаткові дії, наприклад, валідацію отриманих даних чи обробку помилок. Також важливо врахувати безпеку при роботі з HTTP запитами, наприклад, шифрування даних за допомогою HTTPS.

## Дивись також

- [iOS Networking in Swift: Understanding URLSession](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)
- [Apple Developer Documentation: URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Manning: Using Swift to Send HTTP Requests](https://freecontent.manning.com/using-swift-to-send-http-requests/)