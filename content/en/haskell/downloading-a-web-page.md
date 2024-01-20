---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Grabbing the Web with Haskell: A Quick Guide

## What & Why?
Downloading a web page involves saving HTML content from the internet onto local storage. Programmers do this to mine data, automate human browsing activity, or test websites.

## How to:
In Haskell, we achieve this by using the `http-conduit` library. The code becomes quite simple:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://example.com"
    putStrLn $ B.unpack $ getResponseBody response
```

Run the code, and voila! You have the HTML content in your command line.

## Deep Dive
Downloading a web page from Haskell has come a long way. Early on, Haskell programmers had to interface with C libraries to accomplish this task.

Today, `http-conduit`, part of the robust Conduit library, handles HTTP requests like a charm. It provides high-level and low-level tools for making requests, leaves options for extensibility and customization, and even provides HTTPS support!

Apart from `http-conduit`, there are many alternatives in the Haskell ecosystem, like `wreq` and `http-client`, depending on the use case and personal preference.

Remember, when you download a web page, you're essentially making a GET request and storing the response. The server responds to the request with a status line, headers, and a message body - which typically contains the HTML of the web page.

## See Also 
1. Conduit Library: https://hackage.haskell.org/package/conduit
2. http-conduit documentation: https://hackage.haskell.org/package/http-conduit
3. A great comparison of web scraping libraries in Haskell: https://www.fpcomplete.com/blog/2015/04/web-scraping-haskell