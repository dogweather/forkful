---
title:                "Sending an http request"
html_title:           "Java recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is a way for a programmer to communicate with a web server. It is used to retrieve data or perform an action on the server, making it a crucial part of web development. This allows programmers to build dynamic and interactive websites or applications that can communicate with servers to provide a personalized experience for users.

## How to:
To send an HTTP request in Java, we can use the built-in class `HttpURLConnection` from the `java.net` package. First, we need to create a URL object with the desired URL, then open a connection to that URL using `HttpURLConnection`. Next, we can set the request method, add any headers or parameters, and finally, read the response using an `InputStream`. Here's an example of sending a GET request to a server and reading the response:

```Java
// create URL object
URL url = new URL("https://example.com/api/users");

// open connection
HttpURLConnection con = (HttpURLConnection) url.openConnection();

// set request method
con.setRequestMethod("GET");

// read response
InputStream is = con.getInputStream();

// convert InputStream to String
BufferedReader reader = new BufferedReader(new InputStreamReader(is));
StringBuilder response = new StringBuilder();
String line;
while ((line = reader.readLine()) != null) {
  response.append(line);
}
reader.close();

// print response
System.out.println(response.toString());
```

The output of this code would be the response data from the server in the form of a string.

## Deep Dive:
Initially, sending HTTP requests in Java was done using the `HttpURLConnection` or `HttpClient` classes. However, with the introduction of Java 11, the `HttpRequest` and `HttpClient` classes were added, providing a simpler and more efficient way to make HTTP requests. These classes also support asynchronous requests, making them suitable for high-performance applications.

While Java is a popular choice for sending HTTP requests, there are other alternatives such as Node.js, Python, and Ruby that also have libraries or modules for making HTTP requests. Developers can choose the language they are most comfortable with or consider factors such as performance, community support, and project requirements before deciding on the best option.

Implementing HTTP requests in Java involves understanding HTTP methods, status codes, and headers. It is also essential to handle errors and exceptions that may occur during the request. Additionally, developers can use libraries like `Gson` or `Jackson` to parse the response data into objects or use frameworks like `Spring Boot` to streamline the process of making HTTP requests.

## See Also:
- [Oracle Java 11 documentation on HTTP client](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/package-summary.html)
- [Baeldung tutorial on making HTTP requests with Java 11](https://www.baeldung.com/java-11-http-client)
- [Comparison of different languages for making HTTP requests](https://raygun.com/blog/making-http-requests-node-js-vs-python-vs-ruby/)