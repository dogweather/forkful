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

## Why

Have you ever needed to retrieve data from a web server? Maybe you want to fetch information from an API or simply access a webpage. This is where sending an HTTP request comes in handy. By sending an HTTP request, you can communicate with a server and get the data you need.

## How To

To send an HTTP request in Haskell, we will be using the `Wreq` library which provides a simple API for making HTTP requests. First, we need to import the necessary modules:

```Haskell
import Network.Wreq
import Control.Lens -- for using lens operators
import Data.Text as T -- for working with text data
import Network.HTTP.Client.TLS -- for using https
```

Next, we can make a GET request using the `get` function. This function takes in a URL and returns a response with the data from that URL.

```Haskell
response <- get "https://example.com"
```

To access the data from the response, we will use the `responseBody` lens operator.

```Haskell
response ^. responseBody
```

This will return a `ByteString` value, which we can convert to `Text` if needed.

```Haskell
body <- response ^? responseBody . to strict . unpackedChars
```

Finally, we can print the data to the console using `putStrLn`:

```Haskell
putStrLn body
```

If we want to make a POST request with some data, we can use the `post` function and pass in the URL and the data as a `FormParam`.

```Haskell
response <- post "https://example.com" ["username" := "John", "password" := "12345"]
```

## Deep Dive

There are different methods for sending HTTP requests, such as GET, POST, PUT, DELETE, etc. The `Wreq` library provides functions for each of these methods, making it easy to choose the appropriate one for your needs.

Additionally, you can also specify headers, cookies, and other parameters in your requests using functions provided by the library. This gives you more control and flexibility over your requests.

It's also important to handle errors when making HTTP requests. The `Wreq` library provides functions for checking the response status and handling errors accordingly. These functions can help you make more robust and reliable code.

## See Also

- [Wreq documentation](https://hackage.haskell.org/package/wreq)
- [HTTP clients in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20HTTP%20Client)
- [Handling errors in Haskell](https://www.fpcomplete.com/blog/2017/07/parsing-errors-fall-through)