---
date: 2024-01-20 17:44:03.230611-07:00
description: 'How to: Let''s roll with a straightforward example using Haskell''s
  `http-conduit` library. First, install it using `cabal install http-conduit`. Then.'
lastmod: '2024-03-13T22:45:00.127218-06:00'
model: gpt-4-1106-preview
summary: Let's roll with a straightforward example using Haskell's `http-conduit`
  library.
title: Downloading a web page
weight: 42
---

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
