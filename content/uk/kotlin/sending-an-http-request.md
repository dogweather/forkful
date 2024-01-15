---
title:                "Надсилання http запиту"
html_title:           "Kotlin: Надсилання http запиту"
simple_title:         "Надсилання http запиту"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому
Відсилання HTTP-запиту є необхідним елементом веб-розробки. Це дозволяє програмістам взаємодіяти з сервером та отримувати необхідну інформацію.

## Як
```Kotlin
import java.net.*

// встановлення URL за допомогою строкового значення
val url = URL("https://example.com")

// створення підключення HTTP за допомогою методу openConnection ()
val connection = url.openConnection() as HttpURLConnection

// встановлення типу запиту та коду відповіді
connection.requestMethod = "GET"

// встановлення таймауту та вказівки про використання кешу
connection.connectTimeout = 5000
connection.useCaches = true

// отримання вихідного потоку та зчитування даних з відповіді
val inputStream = connection.inputStream
val response = inputStream.reader().readText()

// закриття підключення
connection.disconnect()

// виведення відповіді на екран
println(response)
```

## Глибоке дослідження
Відправка HTTP-запиту використовує протокол TCP (Transmission Control Protocol), який забезпечує надійну передачу даних через інтернет. Щоб побачити, як саме відбувається процес відправки запиту, можна скористатися інструментами для знімання мережевого трафіку, наприклад Wireshark або Fiddler. Це дозволить відстежувати передачу даних під час взаємодії з сервером.

## Дивіться також
- [Official Kotlin Website](https://kotlinlang.org)
- [Java URL class documentation](https://docs.oracle.com/javase/7/docs/api/java/net/URL.html)
- [Wireshark download](https://www.wireshark.org/download.html)
- [Fiddler download](https://www.telerik.com/download/fiddler)