---
title:                "Haskell recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

In today's digital age, information is readily available at our fingertips. We can access any website or webpage with just a few clicks. As a programmer, there may come a time when you need to download a web page for data extraction, analysis, or simply for offline reading. This is where using Haskell for downloading web pages can come in handy. With its functional and powerful features, Haskell provides an elegant solution for this task.

## How To

To download a web page in Haskell, we need to use the "Network.HTTP" module. This module provides functions for making HTTP requests and handling responses. Let's take a look at a simple example:

```
import Network.HTTP

-- Downloads a web page and prints the response body
main = do
  response <- simpleHTTP (getRequest "https://example.com")
  body <- getResponseBody response
  print body
```

In the above code, we import the "Network.HTTP" module and use its "simpleHTTP" function to make a GET request to the specified URL. We then use the "getResponseBody" function to extract the response body from the "response" variable. Finally, we print the body to the console. Running this code will download and print the source code of the webpage.

We can also add more functionality to our code, such as specifying headers, handling errors, or processing the response data. The Haskell community has developed many useful packages for dealing with web requests, such as "wreq" and "http-conduit". These packages provide more advanced features and make working with HTTP requests even easier.

## Deep Dive

Behind the scenes, the "Network.HTTP" module uses a data type called "Request". This data type represents an HTTP request and contains information such as the request method, headers, and body. The "simpleHTTP" function takes a "Request" as its parameter and returns an "IO" action that contains the response. The "Response" data type, on the other hand, represents an HTTP response and contains the response status code, headers, and body.

Haskell's strong type system and purity make it an excellent choice for web-related tasks. By utilizing monads and powerful data types, we can create robust and maintainable code for downloading web pages.

## See Also

- [Network.HTTP documentation](https://hackage.haskell.org/package/HTTP)
- [wreq package on Hackage](https://hackage.haskell.org/package/wreq)
- [http-conduit package on Hackage](https://hackage.haskell.org/package/http-conduit)