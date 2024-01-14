---
title:                "Java recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Downloading a web page is a fundamental skill for any Java programmer. It allows you to retrieve information from websites and use it in your own programs. Whether you want to build a web crawler, extract data for analysis, or simply display a webpage within your application, knowing how to download a web page is essential.

## How To

To download a web page in Java, we can use the `URL` and `URLConnection` classes from the `java.net` package. First, we need to create a `URL` object with the URL of the webpage we want to download. Then, we can open a connection to that URL using the `openConnection()` method. We can then use the `getInputStream()` method to retrieve the page's content as a stream of bytes. We can then read this stream and convert it to a `String` using the `BufferedReader` class.

```Java
URL url = new URL("https://www.example.com");
URLConnection connection = url.openConnection();
InputStream stream = connection.getInputStream();
BufferedReader reader = new BufferedReader(new InputStreamReader(stream));

String line;
while ((line = reader.readLine()) != null) {
    System.out.println(line);
}

reader.close();
```

This example will print out the HTML code of the webpage to the console. We can also save this content to a file or parse it for specific information, depending on our needs.

## Deep Dive

The `URLConnection` class also provides various methods for setting connection parameters, such as adding request headers and setting timeouts. We can use these methods to customize our connection according to the requirements of the web page we are downloading. Additionally, we can also use the `HttpURLConnection` class which provides methods for handling specific HTTP requests, such as GET, POST, and PUT.

It is important to note that downloading a web page can also involve more complex techniques, such as handling redirects and handling cookies. These topics are beyond the scope of this post, but it is important to keep in mind that there are more advanced techniques that can be used for more specific situations.

## See Also

Here are some additional resources to further your understanding of downloading web pages in Java:

- [Oracle Java Tutorials: Working with URLs](https://docs.oracle.com/javase/tutorial/networking/urls/index.html)
- [Java Network Application Cookbook by Michael C. Daconta](https://www.oreilly.com/library/view/java-network-application/1565928709/)