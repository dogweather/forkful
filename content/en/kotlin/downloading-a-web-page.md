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

## What & Why?
Downloading a web page is the act of retrieving the contents of a webpage from the internet. Programmers do it to access the information on the webpage and use it in their code for various purposes, such as data scraping, automation, or integrating web content into their applications.

## How to:
To download a web page in Kotlin, we can use the `URL` and `BufferedReader` classes from the `java.net` package. First, we need to create a `URL` object with the desired webpage URL. Then, we can use `openStream()` to get the input stream from the URL connection. Next, we can use `BufferedReader` to read the input stream line by line and store the contents in a string variable. Here’s an example of how we can print the contents of a webpage to the console:

```
val url = URL("https://www.example.com")
val inputStream = url.openStream()
val bufferedReader = BufferedReader(InputStreamReader(inputStream))
var inputLine: String?
while (bufferedReader.readLine().also { inputLine = it } != null) {
  println(inputLine)
}
```

Sample output:
```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
…
</html>
```

## Deep Dive:
Downloading web pages has been a fundamental task for web developers since the early days of the internet. Before APIs and web services were widely used, web scraping and manually copying information from web pages were common practices. However, with the rise of programming languages like Kotlin, developers now have the ability to automate this process and handle data in a more efficient way. Some alternatives to using the `java.net` package in Kotlin for downloading web pages include using third-party libraries like `OkHttp` or integrating with web scraping frameworks like `Jsoup`. Implementing the `BufferedReader` in Kotlin gives us access to various methods for reading input streams, such as `readLine()` or `readText()`.

## See Also:
- Official documentation for the `java.net` package in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.jvm/java.net/index.html
- Third-party library for handling HTTP requests in Kotlin: https://square.github.io/okhttp/
- Web scraping framework for Java and Kotlin: https://jsoup.org/