---
title:                "Downloading a web page"
date:                  2024-01-20T17:44:06.931068-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage means grabbing its content, like HTML, CSS, and JavaScript, programmatically. Programmers do this to process data, monitor changes, or test their web apps.

## How to:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String urlStr = "http://example.com";
        try {
            URL url = new URL(urlStr);
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    System.out.println(line);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Sample output might look like this:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>
```

## Deep Dive

Back in the day, downloading a webpage was elementary—HTTP was simple, websites were mostly static HTML. Today's web is complex—think HTTPS, JavaScript-driven content, and AJAX galore. 

For static content, `java.net.URL` and `java.net.HttpURLConnection` are straight-up choices—no-frills, just works. But if you're targeting sites full of dynamic content loaded by JavaScript, those classes won't cut it alone, and you're looking at tools like Selenium or HtmlUnit instead.

Don't forget, picking the right tool also hinges on what you need to do with the page once it's downloaded. Parsing HTML? Jsoup is your friend. Executing JavaScript? Consider a headless browser. The `java.net` classes are just the tip of the iceberg, but they are suitable for quick tasks or data scraping from plain ol' webpages.

Remember the politeness policy: don't pummel a site with rapid-fire requests, or you're asking for a ban. And make sure you’re playing nice with the website’s `robots.txt` guidelines.

## See Also

- The [Jsoup library](https://jsoup.org/) for HTML parsing and extraction.
- The [Selenium WebDriver](https://www.selenium.dev/documentation/en/webdriver/) for more complex tasks including JavaScript execution.
- A guide to [HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html) for those wanting the nitty-gritty on Java's built-in way to handle HTTP.
- [HtmlUnit](http://htmlunit.sourceforge.net/), a "GUI-Less browser for Java programs", great for JavaScript-heavy pages.
