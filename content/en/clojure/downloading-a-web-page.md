---
title:                "Downloading a web page"
html_title:           "Clojure recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

If you're interested in web scraping or data extraction, downloading a web page is an essential step. It allows you to access and manipulate the content of a website for various use cases, such as data analysis or automation.

## How To

To download a web page in Clojure, we can use the `clojure.java.io` library. This library provides functions for input and output operations, including downloading web pages. Let's see an example of how to download a webpage using the `clojure.java.io` library:

```Clojure
(require '[clojure.java.io :as io])

(defn download-webpage [url]
  (with-open [reader (io/reader url)]
    (slurp reader)))

(download-webpage "https://www.example.com")
```

The above code will download the HTML content of the webpage at the specified URL and return it as a string. You can then manipulate this string as needed for your use case.

If you want to save the downloaded webpage as a file, you can use the `io/copy` function, which takes in the source and destination paths as arguments:

```Clojure
(require '[clojure.java.io :as io])

(defn save-webpage [url file-path]
  (with-open [in (io/reader url)
              out (io/output-stream file-path)]
    (io/copy in out)))

(save-webpage "https://www.example.com" "example.html")
```

This will save the webpage at the specified URL as a local file named "example.html".

## Deep Dive

Behind the scenes, the `clojure.java.io` library uses Java's `java.net.URL` and `java.net.URLConnection` to handle the actual downloading of web pages. These classes provide methods for establishing a connection to a URL and retrieving the content.

The `io/reader` and `io/output-stream` functions from the `clojure.java.io` library are just wrappers around the `java.net.URL` and `java.net.URLConnection` APIs, providing a convenient way to access and process the content of a webpage.

## See Also
- [Clojure Documentation: clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Java Documentation: java.net.URL](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Java Documentation: java.net.URLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html)