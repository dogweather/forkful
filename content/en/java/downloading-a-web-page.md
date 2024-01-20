---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means fetching its HTML content via the internet. Programmers do it for tasks like web scraping, monitoring site changes, or offline browsing.

## How to:

Here's an easy way using Java.io package and Java.net package. Make sure your imports look something like this:

```Java
import java.io.BufferedInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
```

And your download function look something like this:

```Java
public static void downloadPage(String webpage, String output) throws IOException {
    BufferedInputStream in = null;
    FileOutputStream out = null;
    try {
        in = new BufferedInputStream(new URL(webpage).openStream());
        out = new FileOutputStream(output);
        byte data[] = new byte[1024];
        int count;
        while ((count = in.read(data, 0, 1024)) != -1) {
            out.write(data, 0, count);
        }
    } finally {
        if (in != null) {
            in.close();
        }
        if (out != null) {
            out.close();
        }
    }
}
```

Remember to call the function with your desired webpage and output file name:

 ```Java
downloadPage("https://example.com", "downloaded.html");
```

This will create a file named "downloaded.html" with webpage content. You might need to handle exceptions according to your specific use-case.

## Deep Dive

Getting data from the web is fundamental to the internet’s function since its inception. However, web page downloading in Java wasn't always this straightforward. Earlier versions of the JDK didn't have support for BufferedInputStream or URL classes, making the process more complex.

An alternative to the java.io package method is the use of HttpClient library added in JDK 11 for HTTP requests. Some third-party libraries like Jsoup or HtmlUnit can also handle web page downloading besides their primary function of parsing HTML.

BufferedInputStream in the code example makes use of a buffer to store data. This buffering allows it to make fewer, but larger, reads from the source, which can be more efficient when reading from slow sources like a network.

## See Also

For additional HTTP methods and features, consider looking into the HttpClient library (JDK 11 onwards): [HttpClient Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)

For scrapers or parser libraries, check out Jsoup: [Jsoup Documentation](https://jsoup.org/) and HtmlUnit: [HtmlUnit Documentation](http://htmlunit.sourceforge.net/)