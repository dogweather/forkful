---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:50.620356-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Swift \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\u0442 \u043B\
  \u0435\u0433\u043A\u043E \u043E\u0442\u043F\u0440\u0430\u0432\u043B\u044F\u0442\u044C\
  \ HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u044B \u0441 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043A\u043B\u0430\u0441\
  \u0441\u0430 `URLSession`. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0439 \u043F\u0440\u0438\u043C\u0435\u0440 GET-\u0437\u0430\u043F\u0440\u043E\u0441\
  \u0430."
lastmod: '2024-03-13T22:44:45.677834-06:00'
model: gpt-4-0125-preview
summary: "Swift \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\u0442 \u043B\u0435\
  \u0433\u043A\u043E \u043E\u0442\u043F\u0440\u0430\u0432\u043B\u044F\u0442\u044C\
  \ HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u044B \u0441 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043A\u043B\u0430\u0441\
  \u0441\u0430 `URLSession`."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
Swift позволяет легко отправлять HTTP-запросы с использованием класса `URLSession`. Вот простой пример GET-запроса:

```Swift
import Foundation

// URL ресурса, который вы запрашиваете
if let url = URL(string: "https://api.example.com/data") {

    // Создать URLSessionDataTask
    let task = URLSession.shared.dataTask(with: url) { data, response, error in
        
        // Проверить наличие ошибки
        if let error = error {
            print("Ошибка получения данных: \(error)")
            return
        }
        
        // Проверить, получен ли допустимый ответ и данные
        if let httpResponse = response as? HTTPURLResponse, 
           httpResponse.statusCode == 200,
           let data = data {
            
            // Преобразовать данные в строку и напечатать
            let dataString = String(decoding: data, as: UTF8.self)
            print(dataString)
        }
    }
    // Запустить задачу
    task.resume()
}

// Пример вывода будет содержимым, полученным из API.
```

Для отправки POST-запроса с JSON:

```Swift
import Foundation
import CoreFoundation

// Ваш конечный API-пункт
if let url = URL(string: "https://api.example.com/submit") {

    // Подготовьте данные, которые хотите отправить
    let dictionary = ["key": "value"]
    guard let jsonData = try? JSONSerialization.data(withJSONObject: dictionary) else {
        print("Ошибка: не удается создать JSON из словаря")
        return
    }
    
    // Подготовить URLRequest
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    request.httpBody = jsonData
    
    // Создать и начать задачу
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        // Обработка ответа здесь
    }
    task.resume()
}

// Вывод зависит от ответа сервера. Стандартный вывод отсутствует.
```

## Погружение в детали:
HTTP-запросы являются основой веб-коммуникаций. Они существуют с первых дней веба, обеспечивая стандартизированный способ обмена данными.

Альтернативы `URLSession` включают сторонние библиотеки, такие как Alamofire, которые упрощают синтаксис и добавляют функциональность. Однако `URLSession` остается родным выбором для сетевых вызовов, и Apple постоянно обновляет его, следуя последним возможностям сетевого взаимодействия и стандартам безопасности.

Важной деталью реализации является то, что сетевые запросы в Swift асинхронны по своей природе. Они выполняются в фоновом режиме, позволяя приложению оставаться отзывчивым. Когда приходит ответ, вызывается обработчик завершения. Крайне важно правильно управлять управлением потоками, особенно при обновлении пользовательского интерфейса, что должно происходить в основном потоке.

## Смотрите также:
- [URLSession | Документация для разработчиков Apple](https://developer.apple.com/documentation/foundation/urlsession)
- [Работа с JSON в Swift](https://developer.apple.com/swift/blog/?id=37)
- [Репозиторий Alamofire на GitHub](https://github.com/Alamofire/Alamofire)
