---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:32.143706-07:00
description: "\u0421\u043A\u0430\u0447\u0438\u0432\u0430\u043D\u0438\u0435 \u0432\u0435\
  \u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438\u0437 \u0441\u0435\u0442\u0438 \u0438\
  \ \u0438\u0445 \u0438\u043C\u043F\u043E\u0440\u0442 \u0432 \u0432\u0430\u0448\u0435\
  \ \u043F\u0440\u0438\u043B\u043E\u0436\u0435\u043D\u0438\u0435. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\
  \u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043F\u043E\u043B\u0443\u0447\u0435\
  \u043D\u0438\u044F \u043A\u043E\u043D\u0442\u0435\u043D\u0442\u0430, \u0432\u0437\
  \u0430\u0438\u043C\u043E\u0434\u0435\u0439\u0441\u0442\u0432\u0438\u044F \u0441\u2026"
lastmod: '2024-03-13T22:44:45.681259-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043A\u0430\u0447\u0438\u0432\u0430\u043D\u0438\u0435 \u0432\u0435\
  \u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438\u0437 \u0441\u0435\u0442\u0438 \u0438\
  \ \u0438\u0445 \u0438\u043C\u043F\u043E\u0440\u0442 \u0432 \u0432\u0430\u0448\u0435\
  \ \u043F\u0440\u0438\u043B\u043E\u0436\u0435\u043D\u0438\u0435."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Давайте используем `URLSession` для выполнения этой задачи. Swift делает это прямо к делу.

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Ошибка:", error)
        return
    }

    if let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) {
        if let mimeType = httpResponse.mimeType, mimeType == "text/html",
           let data = data, let string = String(data: data, encoding: .utf8) {
            print("Содержимое скачанной веб-страницы:")
            print(string)
        } else {
            print("Неверный MIME-тип или кодировка.")
        }
    } else {
        print("Сервер ответил ошибкой.")
    }
}
task.resume()
// Убедитесь, что площадка для игр продолжает работать до завершения задачи
RunLoop.current.run()
```

Примерный вывод может выглядеть так:

```
Содержимое скачанной веб-страницы:
<!doctype html>...
```

## Подробнее
API `URLSession` существует начиная с iOS 7 и macOS 10.9. В то время это было настоящее открытие, заменившее старый и более громоздкий `NSURLConnection`. Хотя `URLSession` мощный и гибкий, вы также можете рассмотреть сторонние библиотеки, такие как Alamofire, для более сложных сетевых нужд.

При реализации помните, что сетевые запросы асинхронны. Это означает, что ваше приложение может продолжать выполнять другие задачи, пока сервер отвечает. Также, правильное использование `URLSession` включает в себя корректную обработку ошибок и проверку статуса ответа сервера. Проверка MIME-типа крайне важна для того, чтобы убедиться, что вы получаете HTML, а не другие типы файлов, такие как JSON или изображение.

## Смотрите также
Углубитесь в тему или изучите альтернативы:
- Документация Apple `URLSession`: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Сетевое взаимодействие в Swift с помощью Alamofire: [Alamofire](https://github.com/Alamofire/Alamofire)
- Использование паттерна async/await для `URLSession` в iOS 15+: [URLSession async/await](https://developer.apple.com/videos/play/wwdc2021/10054/)
