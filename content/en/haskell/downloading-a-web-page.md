---
title:                "Downloading a web page"
html_title:           "Haskell recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page refers to the process of retrieving the content of a webpage from the internet. Programmers often do this to extract information or data from a webpage for use in their own applications or systems.

## How to:

```Haskell
import Network.HTTP.Conduit (simpleHttp)

-- Download webpage
pageContent <- simpleHttp "https://www.example.com"
```

Output: The variable `pageContent` will contain the content of the webpage as a `ByteString` data type.

## Deep Dive:

Downloading web pages has been a common practice since the early days of the internet. It has evolved over the years with various libraries and tools available to facilitate the process in different programming languages. Some alternatives to `simpleHttp` in Haskell include `curl` and `wget`. The `simpleHttp` function is part of the `http-conduit` package, which provides a high-level interface for making HTTP requests. Under the hood, it uses `http-client`, a low-level HTTP client library.

## See Also:

- [http-conduit package documentation](https://hackage.haskell.org/package/http-conduit)
- [http-client library documentation](https://hackage.haskell.org/package/http-client)
- [Alternatives to simpleHttp in Haskell](https://wiki.haskell.org/Network.HTTP.Conduit-Alternatives)