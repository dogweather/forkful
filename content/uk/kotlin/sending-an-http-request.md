---
title:                "Kotlin: Відправлення http-запиту"
simple_title:         "Відправлення http-запиту"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому
Надіслання HTTP запиту є важливим елементом у веб-розробці, який дозволяє отримувати дані з веб-сервера і виконувати різноманітні дії з цими даними. Наприклад, це може бути використано для отримання інформації з API або відправки даних на сервер.

## Як
Для надсилання HTTP запиту використовуються різні бібліотеки, але в цій статті ми розглянемо використання стандартного HTTP клієнта Kotlin для відправки запиту GET і отримання результатів.

```Kotlin
// Створення об'єкту клієнта
val client = HttpClient()

// Відправлення запиту GET
val response = client.get("http://example.com")

// Отримання вихідних даних
val data = response.readText()

// Виведення результатів
println(data)
```

У цьому прикладі ми використовуємо стандартний HTTP клієнт для надсилання запиту GET на URL http://example.com. Потім ми отримуємо вихідні дані і виводимо їх у консоль.

## Глибока глибина
Більш продумане використання HTTP запиту має бути налаштоване, щоб обробляти різні типи запитів та відповідей, включаючи POST, PUT і DELETE запити. Також можна налаштувати заголовки, передавати параметри запиту і обробляти помилки.

Наприклад, якщо ми хочемо відправити POST запит і передати параметри у форматі JSON, ми можемо використовувати такий код:

```Kotlin
// Створення об'єкту клієнта
val client = HttpClient()

// Створення об'єкту JSON з параметрами
val params = JsonObject()
params.addProperty("name", "John")
params.addProperty("age", 25)

// Відправлення POST запиту з параметрами
val response = client.post("http://example.com", params.toString())

// Отримання вихідних даних
val data = response.readText()

// Виведення результатів
println(data)
```

Цей приклад використовує стандартний HTTP клієнт для відправлення POST запиту на URL http://example.com, передавши параметри у форматі JSON. Отримані дані потім виводяться у консоль.

## Дивись також
- [Документація по HTTP клієнту Kotlin](https://ktor.io/clients/http-client.html)
- [Основи використання HTTP запитів](https://www.ibm.com/support/knowledgecenter/en/SSFKSJ_7.5.0/com.ibm.mq.dev.doc/q032410_.htm)
- [Стандартні методи HTTP запитів](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)