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

## Why

Have you ever wanted to access a web page's content without opening a web browser? Or perhaps you need to automate the gathering of data from multiple websites for a project. In both cases, downloading a web page using Java can save you time and effort.

## How To

To download a web page in Java, we first need to import the necessary libraries. We will be using the `URL` and `HttpURLConnection` classes from the `java.net` package.

```Java
import java.net.URL;
import java.net.HttpURLConnection;
```

Next, we need to specify the URL of the web page we want to download and create a `URL` object with that URL.

```Java
String url = "https://www.example.com";
URL webpage = new URL(url);
```

Now, we use the `openConnection()` method to establish a connection to the web page and cast it to a `HttpURLConnection` object.

```Java
HttpURLConnection connection = (HttpURLConnection) webpage.openConnection();
```

We can then set the request method and any other necessary properties, such as timeout and user-agent, before connecting to the web page.

```Java
connection.setRequestMethod("GET");
connection.setConnectTimeout(5000);
connection.addRequestProperty("User-Agent", "Mozilla/5.0");
connection.connect();
```

Finally, we can read the content of the web page by using the `getInputStream()` method and converting it to a `String` using a `BufferedReader`.

```Java
InputStream input = connection.getInputStream();
BufferedReader reader = new BufferedReader(new InputStreamReader(input));
String line;
StringBuilder content = new StringBuilder();
while ((line = reader.readLine()) != null) {
   content.append(line);
}
```

We can then print the content to the console or use it for further processing.

```Java
System.out.println(content);
```

Output:
```html
<!doctype html>
<html>
<head>
   <title>Example Domain</title>
   <meta charset="utf-8"/>
   <meta http-equiv="Content-type" content="text/html; charset=utf-8"/>
   <meta name="viewport" content="width=device-width, initial-scale=1"/>
</head>
<body>
<div>
   <h1>Example Domain</h1>
   <p>This domain is for use in illustrative examples in documents. You may use this
      domain in literature without prior coordination or asking for permission.
   </p>
   <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## Deep Dive

When downloading a web page, there are various protocols and security measures that may need to be taken into consideration. For example, some websites may require authentication or use HTTPS instead of HTTP. In these cases, additional steps would need to be taken in your code to handle these scenarios.

It is also important to note that downloading a web page using Java may not always be the most efficient method. Libraries such as `jsoup` and `HttpClient` provide more advanced functionalities and handle some of the complexities involved with web page downloading.

## See Also
- [Oracle: HttpURLConnection](https://docs.oracle.com/javase/10/docs/api/java/net/HttpURLConnection.html)
- [Baeldung: Guide to Java HttpURLConnection](https://www.baeldung.com/java-http-request)
- [jsoup: Java HTML Parser](https://jsoup.org/)