---
title:                "Завантаження веб-сторінки"
html_title:           "Swift: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому
Завантаження веб-сторінки є важливою частиною розробки програмного забезпечення. Воно дозволяє отримати доступ до онлайн-серверів та отримати потрібну інформацію, що допомагає створювати більш функціональні програми.

## Як це зробити
Для завантаження веб-сторінки використовується клас ```URLSession``` в Swift. Нижче наведено коди, які показують, які кроки потрібно виконати для завантаження та отримання даних з веб-сторінки.

```
let urlString = "https://www.example.com"
if let url = URL(string: urlString) {
    let session = URLSession(configuration: .default)
    let task = session.dataTask(with: url) { (data, response, error) in
        if let data = data {
            // Обробка отриманих даних 
        }
    }
    task.resume()
}
```

Цей код створює ```URLSession``` і виконує запит до URL. При отриманні даних з сторінки, вони можуть бути оброблені у ```dataTask``` блоку.

## Глибокий розбір
Крім основного коду, зазначеного вище, існують різні налаштування та параметри, які можна використовувати для більш точного контролю над завантаженням веб-сторінки. Наприклад, можна задати обмеження на час виконання запиту або певні заголовки для вказівки додаткової інформації серверу. Більш детальну інформацію про ці параметри можна знайти у [документації](https://developer.apple.com/documentation/foundation/urlsession) Swift.

## Дивіться також
- [Робота з URL-адресами в Swift](https://www.hackingwithswift.com/example-code/system/how-to-open-a-url-in-safari)
- [Робота зі світовими мережами в Swift](https://www.raywenderlich.com/7338125-playing-with-urls-in-swift)