---
title:                "Downloading a web page"
aliases:
- /en/kotlin/downloading-a-web-page/
date:                  2024-01-20T17:44:24.285016-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page means grabbing the HTML from a given URL to see or use locally. Programmers do this for stuff like web scraping, offline reading, or automated testing.

## How to:
Let's roll with Kotlin’s `HttpURLConnection` to quickly nab a web page. We'll also use coroutines for smooth background ops. Here's a primer:

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import kotlinx.coroutines.*

fun main() = runBlocking {
    val url = "http://example.com"
    val result = withContext(Dispatchers.IO) {
        downloadWebPage(url)
    }
    println(result)
}

fun downloadWebPage(urlAddress: String): String {
    val url = URL(urlAddress)
    val connection = url.openConnection() as HttpURLConnection
    try {
        connection.connect()
        return connection.inputStream.bufferedReader().use { it.readText() }
    } finally {
        connection.disconnect()
    }
}
```

Sample output:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```
Nice, eh? You’ve got the web page’s HTML!

## Deep Dive
Downloading web pages is as old as the web itself. In the ’90s, folks used command-line tools like `wget` and `curl`. They're still around but when you want more control or to integrate web content fetching into an app, you code it.

In Kotlin, you could use Java's `HttpURLConnection` or libraries like OkHttp or Ktor for a powerful approach with more features. The example above is bare-bones; in real life, you’d think about error handling, redirects, and performance. Maybe add in retries or a timeout? And you can't forget about handling different character encodings and content types. 

You’d also consider threading. Wouldn’t wanna hang the main thread while fetching a giant page, would we? Hence, coroutines - they let your app stay responsive, fetching in the background without breaking a sweat.

## See Also
- **OkHttp**: https://square.github.io/okhttp/
- **Ktor Client**: https://ktor.io/docs/client.html
- **Kotlin Coroutines**: https://kotlinlang.org/docs/coroutines-overview.html
- **Java HttpURLConnection**: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html

That’s the skinny—get the page, be smart about it, and always respect the data and its source. Happy coding!
