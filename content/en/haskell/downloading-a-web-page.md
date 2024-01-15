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

## Why

Have you ever wanted to download a web page and extract information from it? Maybe you want to create a web scraper or build a tool to analyze website data. By learning how to download a web page in Haskell, you can easily access and manipulate online content for various purposes.

## How To

In Haskell, we can use the `HTTP` library to send HTTP requests and retrieve web pages. First, we will need to import the necessary modules:

```Haskell
import Network.HTTP.Simple -- for making HTTP requests
import qualified Data.ByteString.Lazy.Char8 as L8 -- for working with byte strings
```

Next, we can use the `httpLBS` function to make a GET request to a specific URL. This will return a lazy byte string representing the web page content.

```Haskell
response <- httpLBS "https://www.example.com"
```

We can then use the `getResponseBody` function to extract the byte string from the response.

```Haskell
body <- getResponseBody response
```

Here, `body` will contain the raw HTML of the web page. We can use the `L8.unpack` function to convert the byte string into a regular string for easier manipulation.

```Haskell
let html = L8.unpack body
```

We can then use tools like `regex` or `tagsoup` to extract specific information from the HTML.

## Deep Dive

When making an HTTP request, there are various options and parameters that can be set, such as setting headers or retrieving specific parts of the response. The `Network.HTTP.Simple` library provides many useful functions for handling these options. For a more in-depth tutorial on using this library, check out the [HTTP Simple documentation](https://hackage.haskell.org/package/http-client).

See Also

- [HTTP Simple Documentation](https://hackage.haskell.org/package/http-client)
- [Haskell Regex Library](https://hackage.haskell.org/package/regex)
- [Haskell TagSoup Library](https://hackage.haskell.org/package/tagsoup)