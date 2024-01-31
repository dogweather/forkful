---
title:                "Sending an HTTP request"
date:                  2024-01-20T17:59:43.969878-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is the act of asking a web server for data or action. Programmers do it to interact with APIs, grab web content, or communicate between services.

## How to:
Let's get to the fun stuff. You'll need `http-client` and `http-client-tls` packages. Set up your stack and add them to your `package.yaml` or `.cabal` file. Then, run `stack build` or appropriate commands to fetch them.

Here’s a simple GET request:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    L8.putStrLn $ responseBody response
```

This will print the JSON you received from `httpbin.org`.

## Deep Dive
Back in the day, Haskell's HTTP requests were less straightforward, but libraries like `http-client` have simplified the process.

Alternatives? Sure. There's `wreq`, `req`, and others, often with syntactic sugar or extra features. But `http-client` is like that dependable swiss army knife in your drawer – it always gets the job done.

Under the hood, `http-client` uses a `Manager` to handle connections. It's efficient and reuses sockets. You can tune it, but defaults are fine to start.

## See Also
To extend your toolkit, check these out:

- [The `http-client` package](https://www.stackage.org/package/http-client)
- [The `wreq` package for a more modern approach](https://www.stackage.org/package/wreq)
- [Hackage for Haskell libraries](https://hackage.haskell.org/)
