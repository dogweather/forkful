---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is like asking a website server for specific information or data. Programmers do it to interact with web APIs, fetch data from a database, or post user form data, among other things.

## How to:
In Clojure, the 'clj-http.client' library is our go-to. If it's not in your project, add it to the project.clj dependencies:
```Clojure
[clj-http "3.12.3"]
```
For the 'GET' request, we'll use this example:
```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com" 
                           {:headers {"Accept" "application/json"}})]
  (println (:status response))
  (println (:body response)))
```
This will print the HTTP status code and the response body.

The 'POST' request is similar but with an added payload:
```Clojure
(let [response (client/post "http://example.com" 
                            {:body "my data" 
                             :content-type :json})]
  (println (:status response))
  (println (:body response)))
```
This will post "my data" to the server and print the resulting response.

## Deep Dive
As said, we're using the Clj-http library, which emerged around 2011. One of the most loved aspects of this library is its simplicity in making HTTP requests. Logged requests and responses also makes debugging a breeze.

As for alternatives, you might look into 'http-kit' if you need a full-stack HTTP client/server. However, 'clj-http' is generally more beginner-friendly.

In giving you status codes and headers, among other fields, 'clj-http' thinly maps over Java's Apache HttpClient. Features sometimes depend on this underlying implementation.

## See Also
- Clj-http on GitHub: https://github.com/dakrone/clj-http
- Clojure Cookbook’s section on HTTP requests: https://github.com/clojure-cookbook/clojure-cookbook/blob/master/02_general-computing/2-07_send-receive-http.asciidoc
- Http-kit as an alternative library: http://www.http-kit.org/500.html