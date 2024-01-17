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

## What & Why?

Downloading a web page simply means retrieving a webpage from the internet and storing it on your local machine. It is commonly done by programmers to extract information from a webpage that can be used for various purposes such as data analysis and content scraping.

## How to:

The `clojure.java.io` library provides a simple and efficient way to download a web page in Clojure. Here's an example:

```Clojure
(require '[clojure.java.io :as io])
(io/copy (io/input-stream "https://www.example.com") (io/output-stream "example.html"))
```
The above code will download the web page from `https://www.example.com` and save it as `example.html` in your current directory.

## Deep Dive:

Downloading web pages has been a common practice since the early days of the internet. In the past, it was done manually by copying and pasting the HTML code. However, with the advancement of technology, developers have created various tools and libraries to make the process more efficient and automated.

Some alternatives to the `clojure.java.io` library for downloading web pages include [clj-http](https://github.com/dakrone/clj-http) and [http-kit](https://github.com/http-kit/http-kit). These libraries provide additional features like HTTP authentication and automatic handling of redirects.

Behind the scenes, downloading a web page involves making a HTTP request to the specified URL and receiving a response from the server. The `clojure.java.io` library uses the Java standard library for networking to handle these requests.

## See Also:

- [clj-http](https://github.com/dakrone/clj-http)
- [http-kit](https://github.com/http-kit/http-kit)
- [Java standard library for networking](https://docs.oracle.com/javase/8/docs/api/java/net/package-summary.html)