---
title:                "Att tolka html"
html_title:           "Haskell: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTML parsing är processen med att ta en bit HTML-kod och konvertera den till ett träddiagram som kan bearbetas av ett program. Detta är användbart för att extrahera information från webbsidor eller skapa egna skräddarsydda HTML-dokument.

## Hur fungerar det?
Haskell har ett bibliotek som heter html-parser som gör HTML parsing ganska enkelt. Nedan följer ett exempel på hur man använder detta bibliotek för att extrahera alla länkar från en webbsida:

```Haskell
import Text.HTML.Parser (parseTokens)
import Text.HTML.TagSoup (Tag(TagOpen), isTagOpenName, TagName(Link))
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

getLinks :: String -> IO [String]
getLinks url =
  simpleHTTP (getRequest url) >>= getResponseBody
    >>= return . filter (isTagOpenName "a") . parseTokens
    >>= return . map (fromAttrib "href") . filter (isTagOpenName "a")

-- Exempel: Hämtar alla länkar från Google-sidan
getLinks "https://www.google.com"
-- ["https://mail.google.com","https://www.google.se/imghp?hl=sv&tab=wi"]
```

## Djupdykning
Förutom html-parser biblioteket finns det också andra alternativ för HTML parsing i Haskell, såsom tagsoup och hxt. Dessa bibliotek erbjuder olika funktioner och prestanda, så det är värt att utforska dem för att hitta det som passar bäst för ditt projekt.

Implementationen av html-parser biblioteket är baserad på det tredjeparts biblioteket tagsoup. Det använder en parserkombinatorer för att bygga upp trädet av HTML-taggar och använda dessa för att extrahera information.

## Se även
- [html-parser library](https://hackage.haskell.org/package/html-parser)
- [tagsoup library](https://hackage.haskell.org/package/tagsoup)
- [hxt library](https://hackage.haskell.org/package/hxt)