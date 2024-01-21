---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:44:20.295670-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)

Downloading a web page means grabbing the data from a website's server and making it available on your local machine. Programmers do it for things like data scraping, offline access, or automated testing.

## How to: (Як зробити:)

To download a web page in Java, use `java.net.HttpURLConnection`. Here’s a concise example:

```java
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;

public class WebPageDownloader {
    
    public static void main(String[] args) throws IOException {
        URL url = new URL("http://www.example.com");
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        } finally {
            connection.disconnect();
        }
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

## Deep Dive (Поглиблений аналіз):

Downloading web pages has come a long way from the days of raw socket connections. Now, `HttpURLConnection` is the basic Java class for it, but it’s not the only tool. Libraries like Apache HttpClient or Jsoup provide more functionality and are more user-friendly.

There’s a trade-off: `HttpURLConnection` is built-in and doesn’t need extra jars, while third-party libraries require managing dependencies but can simplify tasks like handling cookies, or managing sessions.

Historically, Java’s `HttpURLConnection` has been criticized for being clunky and less intuitive compared to what modern libraries offer. Yet it remains a go-to option for many, especially when sticking to the standard library is appealing or for simpler tasks where an external library might be overkill.

## See Also (Дивіться також):

For more info, check out:

- Jsoup: https://jsoup.org/ (great for HTML parsing and web scraping)
- Apache HttpClient: http://hc.apache.org/httpcomponents-client-ga/ (feature-rich HTTP client)
- The java.net package documentation: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/net/package-summary.html (for insights into Java’s networking capabilities)