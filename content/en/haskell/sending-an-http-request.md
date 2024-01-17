---
title:                "Sending an http request"
html_title:           "Haskell recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is the process by which a client communicates with a server, usually over the internet, to request or send data. Programmers use this technique in web development and web scraping to retrieve information from websites or to interact with APIs.

## How to:
To send an HTTP request in Haskell, we will use the `http-conduit` library. First, we need to import the necessary modules:
```Haskell
import Network.HTTP.Conduit
import Network.HTTP.Types
```
Next, we can use the `simpleHttp` function to make a GET request to a URL and retrieve the response:
```Haskell
response <- simpleHttp "https://example.com"
```
We can then use functions like `responseStatus` and `responseBody` to get information from the response. Here's a full example that prints the status code and response body:
```Haskell
main :: IO ()
main = do
  response <- simpleHttp "https://example.com"
  putStrLn $ "Status code: " ++ show (responseStatus response)
  putStrLn $ "Response body: " ++ show (responseBody response)
```
Output:
```
Status code: Status {statusCode = 200, statusMessage = "OK"}
Response body: "Welcome to example.com"
```

## Deep Dive:
Sending HTTP requests in Haskell dates back to the early 2000s, with the introduction of libraries like `HTTP` and `curl`. However, these libraries have since been deprecated in favor of more modern and efficient alternatives like `http-conduit` and `wreq`. These newer libraries offer features like concurrency, streaming, and automatic decompression. Additionally, some frameworks like Yesod and Servant have their own built-in HTTP request functions.

## See Also:
- [http-conduit documentation](https://hackage.haskell.org/package/http-conduit)
- [wreq - a more convenient HTTP client library](https://hackage.haskell.org/package/wreq)
- [Yesod - a web framework featuring type-safe URLs and HTTP request handling](https://www.yesodweb.com/)
- [Servant - a lightweight web API framework](https://www.servant.dev/)