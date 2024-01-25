---
title:                "Sending an HTTP request"
date:                  2024-01-20T17:59:08.444920-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is how your program asks another system for data or services over the web. Programmers do it to interact with web APIs, fetch resources, or communicate between services.

## How to:
In Clojure, you can send HTTP requests using the `clj-http` client.

First, add the dependency to your `project.clj`:
```clojure
[clj-http "3.12.3"]
```

Now, let's send a GET request:
```clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://httpbin.org/get")]
  (println response))
```

Output sample:
```clojure
{:status 200, :headers {...}, :body "..."}
```

To post data:
```clojure
(let [response (client/post "http://httpbin.org/post" {:form-params {:key "value"}})]
  (println response))
```

## Deep Dive
Sending HTTP requests isn't new. It's as old as the web itself. Clojure, being a modern Lisp, has several libs to make HTTP requests. `clj-http` is a popular one, but others like `http-kit` or Clojure's core `clj-http.client` exist.

`clj-http` leans on the Apache HttpComponents Client for Java under the hood. It's versatile but can feel Java-heavy. An alternative, `http-kit`, is more lightweight and Clojure-idiomatic but less feature-rich.

When you send HTTP requests, you're doing so over TCP/IP, which frames your requests according to a well-established protocol. This universal standard lets you interact with practically any web service out there.

## See Also
- `clj-http` GitHub repository: https://github.com/dakrone/clj-http
- Official Clojure site: https://clojure.org
- HttpComponents Client documentation: https://hc.apache.org/httpcomponents-client-ga/
- For real-time needs, consider `http-kit`: http://www.http-kit.org