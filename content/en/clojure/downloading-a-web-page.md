---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means fetching its content over the internet - taking all its HTML, CSS, and JavaScript data, and pushing it down to your local machine. Programmers do this mainly to process or analyze that page's data.

## How to:

Clojure offers `clj-http` library for HTTP requests. This is how we download a page:

```Clojure
(ns my-namespace
  (:require [clj-http.client :as client]))

(let [response (client/get "http://example.com")]
  (println (:status response)
           (:body response)))
```

We require `clj-http.client`, make a GET request and print the status and body of the response. 

## Deep Dive

Clojure's HTTP tooling has evolved over time. Initially, many used the built-in Java libraries. Now, the most common approach is the `clj-http` library, which provides a Clojure-ish interface to Apache's HTTP components.

There are alternatives. You could use `http-kit`, a minimal, high-performance Clojure HTTP library. Also, for processing the downloaded data, you may prefer `jsoup`.

Implementation-wise, downloading a web page is simply sending a GET request to the server and waiting for a response. Behind the scenes, it’s more complex - handling HTTP headers, redirects, errors, and streaming.

## See Also

- [clj-http GitHub](https://github.com/dakrone/clj-http)
- [jsoup: Java HTML Parser](https://jsoup.org/)
- [HTTP Kit](http://www.http-kit.org/)

Feel free to dive even deeper into these topics, they're essential for efficient web automation in Clojure.