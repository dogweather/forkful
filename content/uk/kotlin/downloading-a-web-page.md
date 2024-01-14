---
title:                "Kotlin: Завантаження веб-сторінки."
simple_title:         "Завантаження веб-сторінки."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки є важливим кроком у розробці веб-додатків та скриптів. Це дозволяє програмістам отримувати потрібну інформацію з інших веб-сторінок та обробляти її для подальшого використання.

## Як

Кotlin має потужну бібліотеку для завантаження веб-сторінок - `URL` та `HttpURLConnection`. Ось приклад коду, який показує, як завантажити веб-сторінку та отримати її контент:

```Kotlin
val url = URL("https://www.example.com")
val connection = url.openConnection() as HttpURLConnection

connection.requestMethod = "GET"

val responseCode = connection.responseCode
Log.d("HTTP Response Code: ", responseCode.toString())

val input = BufferedReader(InputStreamReader(connection.inputStream))
val result = StringBuilder()

var line = input.readLine()
while (line != null) {
    result.append(line)
    line = input.readLine()
}

val content = result.toString()
Log.d("HTML Content: ", content)
```

Цей код використовує `URL` для створення з'єднання з веб-сторінкою, а потім `HttpURLConnection` для відправлення запиту і отримання контенту. Результат може бути оброблений та використаний згідно з потребами програміста.

## Глибока занурення

Якщо потрібно більш складний або ефективний підхід до завантаження веб-сторінок, Kotlin також пропонує інші інструменти, такі як `OkHttp` та `Jsoup`. `OkHttp` є швидким та високопродуктивним засобом для роботи з мережевими запитами, а `Jsoup` дозволяє легко отримувати та обробляти HTML-код сторінки.

Незалежно від того, який підхід ви виберете, завантаження веб-сторінки є важливою частиною багатьох проектів на Kotlin та допомагає програмістам отримувати необхідну інформацію для своїх програм.

## Дивись також

- [Офіційна документація Kotlin з завантаження веб-сторінок](https://kotlinlang.org/docs/reference/networking.html#urls-and-uriconnections)
- [Офіційна документація OkHttp](https://square.github.io/okhttp/)
- [Офіційна документація Jsoup](https://jsoup.org/)