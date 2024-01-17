---
title:                "Надсилання http-запиту"
html_title:           "Swift: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

Що & Чому?
Надісилання HTTP-запиту - це процес взаємодії між вашою програмою і сервером через Інтернет. Програмісти роблять це для отримання необхідної інформації з сервера або виконання певних дій на ньому.

Як це зробити:
```Swift
let url = URL(string: "https://example.com") // створюємо об'єкт URL з посиланням на сервер
guard let unwrappedURL = url else { // перевіряємо, чи не є посилання некоректним
    print("Invalid URL")
    return
}
let request = URLRequest(url: unwrappedURL) // створюємо об'єкт запиту за допомогою URL
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in // створюємо завдання для виконання запиту
    if let data = data { // перевіряємо, чи отримали дані відповіді
        print(data) // виводимо дані у консоль або обробляємо їх подальше
    }
}
task.resume() // запускаємо завдання
```

Глибше пірнання:
Надіслання HTTP-запиту є однією з найпоширеніших технік для взаємодії з сервером в сучасному програмуванні. Це може бути корисно при отриманні даних для вашої програми або для виконання деяких дій на сервері. Є також альтернативні методи взаємодії з сервером, наприклад WebSocket або REST, які використовуються для відправки та отримання даних в реальному часі. Щоб виконати HTTP-запит, ви можете використовувати різні інструменти, такі як URLSession у Swift, AFNetworking у Objective-C або Retrofit у Java.

Дивіться також:
https://developer.apple.com/documentation/foundation/url_loading_system - офіційна документація з URL Loading System
https://www.raywenderlich.com/1485153-what-is-an-api-in-plain-english - стаття про те, що таке API і як використовувати його для надсилання HTTP-запитів