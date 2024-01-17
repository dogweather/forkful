---
title:                "Sending an http request"
html_title:           "Clojure recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request refers to the process of a client (such as a web browser) requesting information from a server over the internet using the HTTP protocol. Programmers use this to retrieve data or trigger actions on a remote server, enabling them to create dynamic and interactive web applications.

## How to:
To send an HTTP request in Clojure, we can use the `clojure.java.io` namespace and the `put` function. Here's an example of how we can send a simple GET request and print the response code and body:

```Clojure
(require '[clojure.java.io :as io])
(let [request (io/request "http://example.com")]
    (println "Response Code:" (:status request))
    (println "Response Body:" (:body request)))
```

The output should look something like this:

```Clojure
Response Code: 200
Response Body: "<html>..."
```

## Deep Dive:
HTTP requests have been a fundamental part of web development since the early days of the internet. They are a crucial aspect of the client-server model, allowing communication between a client and a server over the web.

Apart from the `clojure.java.io` namespace, there are a few other popular libraries in Clojure for sending HTTP requests, such as `clj-http` and `http-kit`. These libraries provide more advanced features, such as handling cookies and redirects, and are often used in production-level applications.

Under the hood, the `put` function uses Java's `java.net.HttpURLConnection` class to create and handle the request. Clojure's `with-open` macro ensures that the connection is properly closed after the request is completed.

## See Also:
- [ClojureDocs - clojure.java.io](https://clojuredocs.org/clojure.java.io/request)
- [ClojureVerse - HTTP Libs library comparison](https://clojureverse.org/t/http-libs-library-comparison/1044)
- [Java docs - HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)