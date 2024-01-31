---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:44:25.783910-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Downloading a web page means grabbing the data of a site. Programmers do it to parse data, monitor changes, or to interact with web services.

## Як це зробити:
We use `HttpURLConnection` to fetch web content in Kotlin. Here’s the bare-bones way to do it:

```Kotlin
import java.net.HttpURLConnection
import java.net.URL

fun downloadWebPage(url: String): String {
    val connection = URL(url).openConnection() as HttpURLConnection
    return try {
        connection.inputStream.bufferedReader().use { it.readText() }
    } finally {
        connection.disconnect()
    }
}

fun main() {
    val webContent = downloadWebPage("http://example.com")
    println(webContent.substring(0, 100)) // print first 100 chars
}
```

Sample output:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Поглиблений Розбір:
Back in the day, fetching a web page meant lots of manual work with sockets. Libraries like Apache's `HttpClient` simplified it, and Kotlin's extension functions make the syntax even cleaner.

Nowadays, `HttpURLConnection` is the raw way to handle HTTP in Java/Kotlin but not the only one. OkHttp and Retrofit are modern alternatives offering more features and simpler, more readable code.

Details matter. While `HttpURLConnection` auto-handles redirects, handling errors and timeouts needs extra code. Be mindful of UTF-8 and content parsing. Tools like Jsoup can save your day when working with HTML.

Lastly, keep in mind networking tasks should run on background threads to keep UIs responsive.

## Також Гляньте:
- Kotlin docs for networking: https://kotlinlang.org/docs/networking.html
- OkHttp’s official site: https://square.github.io/okhttp/
- Retrofit’s official site: https://square.github.io/retrofit/
- Jsoup for HTML parsing: https://jsoup.org

Happy coding, and remember to check the rules regarding web scraping and the legality in your context!
