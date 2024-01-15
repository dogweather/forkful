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

## Why
Sending an HTTP request is essential for interacting with web servers and retrieving data from remote sources. It allows developers to build powerful and dynamic applications that can communicate with other systems on the internet.

## How To
To send an HTTP request in Clojure, we will use the `clj-http` library. First, we need to add it as a dependency in our project.clj file:
```Clojure
:dependencies [[clj-http "3.11.0"]]
```
Next, we need to require the `clj-http.client` namespace in our code:
```Clojure
(ns my-app.core
  (:require [clj-http.client :as http]))
```
Now, we can use the `http/get` function to make a GET request to a specific URL and retrieve the response:
```Clojure
(def response (http/get "https://www.example.com"))
```
We can also pass additional parameters to the `http/get` function, such as headers and query parameters:
```Clojure
(def response (http/get "https://www.example.com"
                        {:headers {"Content-Type" "application/json"}
                         :query-params {:page 1 :limit 10}}))
```
To make a POST request, we can use the `http/post` function and pass in the request body:
```Clojure
(def response (http/post "https://www.example.com/users"
                         {:body {:name "John" :email "john@example.com"}}))
```
The `response` variable will contain a map with the status code, headers, and body of the HTTP response. We can access these values using the `:status`, `:headers`, and `:body` keys, respectively.

## Deep Dive
Clojure's `clj-http` library is built on top of the Apache HttpComponents project, which provides a powerful HTTP client for Java. This means that we have access to a wide range of configuration options and features, such as SSL/TLS support, connection pooling, and cookies management.

In addition to the basic HTTP methods (GET, POST, PUT, DELETE), the `clj-http` library also supports other request methods, including PATCH, HEAD, and OPTIONS. We can specify the method in the request options using the `:method` key.

Sending HTTP requests asynchronously is also possible with `clj-http`, using the `http/async-get` and `http/async-post` functions. These functions return a `future` object, which is useful for handling long-running requests without blocking the main thread.

## See Also
- [clj-http documentation](https://github.com/dakrone/clj-http)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/): A beginner-friendly guide to HTTP protocol
- [Ring](https://github.com/ring-clojure/ring): A popular web application library that also provides HTTP request and response handling functionalities.