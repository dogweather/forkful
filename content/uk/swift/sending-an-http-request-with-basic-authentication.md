---
title:                "Надсилання HTTP-запиту з базовою аутентифікацією"
html_title:           "Swift: Надсилання HTTP-запиту з базовою аутентифікацією"
simple_title:         "Надсилання HTTP-запиту з базовою аутентифікацією"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

За допомогою відправлення HTTP-запиту з основною аутентифікацією можна отримати доступ до захищених ресурсів, які вимагають підтвердження ідентифікації користувача. Це може бути потрібно, наприклад, для отримання даних з API або для авторизації на веб-сайті.

## Як це зробити

Щоб відправити HTTP-запит з основною аутентифікацією використовуємо функцію `URLRequest` зі встановленим параметром аутентифікації типу `URLAuthenticationChallenge`. Для цього використовуються ідентифікатор і пароль, які передаються у заголовку `Authorization`. Давайте розглянемо цей приклад коду, який допоможе нам краще зрозуміти процес.

```Swift
// Створюємо базовий URL
let url = URL(string: "https://example.com/api")!

// Створюємо запит
var request = URLRequest(url: url)

// Встановлюємо HTTP-метод і параметри запиту
request.httpMethod = "GET"
request.setValue("application/json", forHTTPHeaderField: "Content-Type")

// Додаємо аутентифікацію
let username = "user"
let password = "password"
let loginString = "\(username):\(password)"
let base64LoginString = Data(loginString.utf8).base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Створюємо сесію для отримання даних
let session = URLSession(configuration: .default)

// Виконуємо запит і отримуємо відповідь
let task = session.dataTask(with: request) { (data, response, error) in
    // Обробляємо отримані дані
    if let error = error {
        print("Помилка: \(error)")
    } else if let data = data,
        let response = response as? HTTPURLResponse,
        response.statusCode == 200 {
        // Обробляємо отримані дані, якщо все в порядку
        print("Отримані дані: \(data)")
    }
}
task.resume()
```

Якщо у нас є успішне підключення і аутентифікація пройшла успішно, у виводі буде міститися отримана відповідь. У противному випадку з'явиться повідомлення про помилку.

## Глибока занурення

HTTP-аутентифікація дозволяє перевірити ідентифікацію користувача перед доступом до захищених ресурсів. Завдяки основній аутентифікації ми можемо передавати ідентифікатор і пароль у закодованому форматі через заголовок `Authorization`, забезпечуючи більшу безпеку захищених даних. Крім того, HTTP-аутентифікація є стандартом, що підтримується більшістю серверів, що робить її універсальним інструментом для отримання доступу