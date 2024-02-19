---
aliases:
- /en/haskell/downloading-a-web-page/
date: 2024-01-20 17:44:03.230611-07:00
description: "Downloading a web page means grabbing its data over the internet; it's\
  \ like saving a copy to read or process locally. Programmers do it to scrape content,\u2026"
lastmod: 2024-02-18 23:09:11.094851
model: gpt-4-1106-preview
summary: "Downloading a web page means grabbing its data over the internet; it's like\
  \ saving a copy to read or process locally. Programmers do it to scrape content,\u2026"
title: Downloading a web page
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means grabbing its data over the internet; it's like saving a copy to read or process locally. Programmers do it to scrape content, interact with web services, or mirror sites.

## How to:

Let's roll with a straightforward example using Haskell's `http-conduit` library. First, install it using `cabal install http-conduit`. Then:

```Haskell
import Network.HTTP.Conduit -- The main network library
import qualified Data.ByteString.Lazy as L -- We'll need Lazy ByteStrings

-- Function to download a web page
downloadPage :: String -> IO L.ByteString
downloadPage url = simpleHttp url

main :: IO ()
main = do
    -- Use the function to download a page
    content <- downloadPage "http://example.com"
    -- Do something with the content, like printing it
    L.putStr content
```

Running this, you'll see the HTML of `http://example.com` on your screen.

## Deep Dive

HTTP requests in Haskell haven't always been this neat. Older libraries like `HTTP` required more boilerplate code. With `http-conduit`, the complexity is abstracted away.

Other methods exist, like the `wget` command in a shell script or Python's `requests` library. But these aren't always as efficient or expressive in Haskell's functional setting.

Under the hood, `http-conduit` uses a Manager to handle connection pooling and Keep-Alive for HTTP1.1, making it more efficient for multiple requests.

## See Also

- For more advanced usage of `http-conduit`: [http-conduit on Hackage](https://hackage.haskell.org/package/http-conduit)
- To understand ByteString: [ByteString on Hackage](https://hackage.haskell.org/package/bytestring)
