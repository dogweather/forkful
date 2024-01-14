---
title:                "Haskell recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
Have you ever wondered how your favorite websites and apps communicate with servers to retrieve information? This is done through the use of HTTP requests, a key component of web development. In this blog post, we will explore how to send HTTP requests using Haskell, a popular functional programming language.

## How To
To send an HTTP request using Haskell, we will use the "http-conduit" library, which provides a high-level interface for working with HTTP requests. To install this library, we can use the following command in our terminal:

```Haskell
cabal install http-conduit
```

Once the library is installed, we can import it into our code and begin making HTTP requests. Here is a simple example of how to make a GET request to the Google homepage:

```Haskell
import Network.HTTP.Conduit( simpleHttp )

main = do
    response <- simpleHttp "https://www.google.com"
    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Response body: " ++ show (getResponseHeaders response)
```

Running this code will print out the status code and response headers of the request to the console. We can see that the status code is "200", indicating that the request was successful, and the response headers contain information about the response, such as the content type and server information.

## Deep Dive
HTTP requests are made up of two parts: the request and the response. The request contains information about the type of request (GET, POST, PUT, DELETE), the headers, and the body. The response contains the status code, headers, and body of the server's response.

In the code example above, we only made a simple GET request, but we can also make other types of requests using the "http-conduit" library. For example, to make a POST request with a body, we can use the "urlEncodedBody" function like this:

```Haskell
import Network.HTTP.Conduit
import Data.ByteString.Char8( pack )

main = do
    let body = pack "name=John&age=25"
    request <- parseUrl "https://www.example.com/users"
    response <- withManager $ httpLbs (urlEncodedBody [("name","John"),("age","25")] request)
    putStrLn "Request made successfully!"
```

This code will send a POST request to the URL with the specified body, which will contain name and age parameters. We can also specify headers, cookies, and other options in our HTTP request using functions provided by the "http-conduit" library.

## See Also
- [http-conduit documentation](https://hackage.haskell.org/package/http-conduit)
- [Introduction to making HTTP requests in Haskell](https://www.haskellforall.com/2012/07/http-doing-requests-to-web-servers.html)
- [Official Haskell website](https://www.haskell.org/)