---
title:                "Завантаження веб-сторінки"
html_title:           "Kotlin: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Чому та для чого?

Завантаження веб-сторінок - це процес отримання коду та вмісту сторінки з Інтернету. Програмісти часто використовують його для створення або оновлення додатків, які залежать від інформації з Інтернету.

Як це зробити:

```Kotlin
import java.net.URL

fun main() {
    val url = URL("https://www.example.com")
    val connection = url.openConnection()
    val inputStream = connection.getInputStream()
    val inputString = inputStream.reader().use { it.readText() }
    println(inputString)
}
```

В результаті виконання цього коду ви отримаєте HTML-код сторінки в консолі.

Глибоке занурення:

1. Історичний контекст: Завантаження веб-сторінок було важливою частиною розробки з самого початку, коли Інтернет тільки починали використовувати.
2. Альтернативи: Є багато інших способів завантаження веб-сторінок, наприклад, використання бібліотек, які спрощують процес, або використання інструментів для автоматичного тестування.
3. Деталі реалізації: Код, наведений в прикладі, використовує клас URL для представлення посилання на сторінку та клас HttpURLConnection для підключення до сервера та отримання відповіді.

Див. також:

- [часопис "Java Code Geeks"](https://www.javacodegeeks.com/2019/02/how-to-download-a-file-from-a-url-in-kotlin.html)
- [документація Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.net.-u-r-l/connection.html)