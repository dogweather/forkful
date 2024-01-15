---
title:                "Downloading a web page"
html_title:           "Kotlin recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

If you've ever wanted to download a web page, whether it's for archiving purposes, offline viewing, or simply to save an interesting article, Kotlin offers a simple solution.

## How To

To download a web page in Kotlin, we can use the `URL` and `BufferedReader` classes from the Java API. First, we need to import these classes:

```Kotlin
import java.net.URL
import java.io.BufferedReader
```

Next, we'll specify the URL of the web page we want to download and use the `URL` class to open a connection to that URL:

```Kotlin
val url = URL("https://www.example.com")
val connection = url.openConnection()
```

Once we have a connection, we can read the contents of the web page using a `BufferedReader` and its `readLine()` method in a loop:

```Kotlin
val reader = BufferedReader(InputStreamReader(connection.inputStream))
var inputLine: String?

while (reader.readLine().also { inputLine = it } != null) {
    println(inputLine)
}
```

This will print out the HTML source code of the web page. We can also store this content in a variable for further processing:

```Kotlin
val content = StringBuilder()
while (reader.readLine().also { inputLine = it } != null) {
    content.append(inputLine)
}
```

And that's it! We now have the web page downloaded and stored in the `content` variable.

## Deep Dive

When downloading a web page, there are a few things to keep in mind. First, the URL must be valid and accessible. If the web page requires authentication, we can use the `URLConnection` class to send authentication credentials. Additionally, the `BufferedReader` has methods for handling timeouts and redirections.

It is also important to note that downloading a web page can be subject to copyright laws, so it's important to check the terms of use of the website before downloading any content.

## See Also

- [Kotlin documentation on URL and BufferedReader](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.net.-u-r-l/index.html)
- [Java documentation on URL and BufferedReader](https://docs.oracle.com/javase/7/docs/api/java/net/URL.html)
- [Download a file from URL using Kotlin](https://www.techiedelight.com/download-file-from-url-kotlin/)