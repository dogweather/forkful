---
title:                "Downloading a web page"
html_title:           "Java recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page involves retrieving the code and content of a webpage from the internet. This is typically done through a connection request using a URL. Programmers often download web pages to extract specific information or to analyze the code for testing or debugging purposes.

## How to:

To download a web page in Java, we can use the `URL` and `URLConnection` classes from the `java.net` package. Here's an example of how we can retrieve the HTML code of a page:

```Java
// Import necessary classes
import java.net.URL;
import java.net.URLConnection;

// Create a URL object for the page we want to download
URL url = new URL("https://www.example.com");

// Open a connection to the URL
URLConnection connection = url.openConnection();

// Retrieve the HTML code from the page
InputStream in = connection.getInputStream();

// Create a BufferedReader to read the input stream
BufferedReader reader = new BufferedReader(new InputStreamReader(in));

// Read the HTML code line by line and print it to the console
String inputLine;
while ((inputLine = reader.readLine()) != null) {
    System.out.println(inputLine);
}

// Close the connection and reader resources
reader.close();
in.close();
```

The output of the above code will be the HTML code of the webpage, which we can then manipulate as needed for our purposes.

## Deep Dive:

Downloading web pages has been a common practice in web development since the early days of the internet. It allows developers to access the code and content of a webpage for various purposes such as data scraping, testing, and debugging.

Apart from using the `java.net` package, there are other alternatives for downloading web pages in Java. One popular option is to use the Apache HttpComponents library, specifically the `HttpClient` class, which provides a more comprehensive API for making HTTP requests.

In the example above, we used the `URLConnection` class to establish a connection to the URL and retrieve the webpage's code. Under the hood, Java uses the HTTP protocol to make the connection. This also means that we can use the same code to retrieve data from other types of URLs, such as API endpoints or FTP servers, by simply changing the URL object.

## See Also:

- [Oracle's official documentation on the `java.net` package](https://docs.oracle.com/javase/10/docs/api/java/net/package-summary.html)
- [How to use the Apache HttpClient library for downloading web pages in Java](https://www.baeldung.com/java-http-client)