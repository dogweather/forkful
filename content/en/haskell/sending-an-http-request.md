---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Untangling HTTP Requests in Haskell

## What & Why?

Sending an HTTP request is a way to retrieve data from servers. Programmers do it to interact with web services, fetch information, or pass data between different parts of an application.

## How to:

To send HTTP requests in Haskell, we rely on the `http-conduit` library. You'll need to install it using cabal: `cabal install http-conduit`.

Here's a simple GET request:

```Haskell
import Network.HTTP.Conduit

main = simpleHttp "http://www.google.com" >>= 
       putStrLn . take 100 . show
```

This program fetches http://www.google.com and print the first hundred characters of the returned HTML.

## Deep Dive

Historically, Haskell's HTTP libraries have not been the easiest to use. This has changed with `http-conduit`, which offers a simplified interface to HTTP connections.

When you attempt to perform an HTTP request in Haskell, you could alternatively use the `http-client` library. While `http-conduit` proposes a simpler to use high-level interface, `http-client` gives you more control over the requests you send.

When sending an HTTP request, the key thing to know is that Haskell sends Internet data over a `Socket`, which is a type of IO channel. The mechanics of forming HTTP headers, parsing responses, and handling errors are all encapsulated within the libraries mentioned.

## See Also

For more in-depth information, consult the http-conduit library on Hackage: https://hackage.haskell.org/package/http-conduit. The Real World Haskell book also covers HTTP programming: http://book.realworldhaskell.org/read/extended-example-web-client-programming.html. Moreover, check out the http-client library: https://hackage.haskell.org/package/http-client.