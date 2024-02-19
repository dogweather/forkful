---
aliases:
- /sv/haskell/parsing-html/
date: 2024-01-20 15:31:59.808672-07:00
description: "Att parsa HTML inneb\xE4r att man omvandlar HTML-str\xE4ngar till en\
  \ datastruktur som datorn kan jobba med. Programmerare g\xF6r detta f\xF6r att enkelt\
  \ kunna\u2026"
lastmod: 2024-02-18 23:08:51.835094
summary: "Att parsa HTML inneb\xE4r att man omvandlar HTML-str\xE4ngar till en datastruktur\
  \ som datorn kan jobba med. Programmerare g\xF6r detta f\xF6r att enkelt kunna\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML innebär att man omvandlar HTML-strängar till en datastruktur som datorn kan jobba med. Programmerare gör detta för att enkelt kunna extrahera information, manipulera innehåll eller integrera webbinnehåll i sina applikationer.

## Hur gör man:
För att parsa HTML i Haskell kan vi använda sig av biblioteket `hxt`, som står för Haskell XML Toolbox. Här är en snabb uppvisning:

```Haskell
import Text.XML.HXT.Core

main :: IO ()
main = do
    let html = "<html><body><p>Hello, world!</p></body></html>"
    -- Parse the HTML and get all paragraph contents
    contents <- runX $ readString [withParseHTML yes, withWarnings no] html
                    >>> deep (isElem >>> hasName "p")
                    >>> getChildren >>> getText
    mapM_ putStrLn contents
```

Detta kommer att skriva ut:

```
Hello, world!
```

## Djupdykning:
HTML-parsning i Haskell började få uppmärksamhet när webbutveckling blev mer funktionell. `hxt` är inte det enda biblioteket för att parsa HTML. Alternativ inkluderar `tagsoup` och `pandoc` för olika användningsområden, från lätta till tunga parsinguppgifter. `hxt` använder en kombination av laziness och arrow syntax för att hantera XML-data, vilket passar bra för Haskell's funktionella paradigm.

I parsingprocessen omvandlar `hxt` HTML-dokumentet till en trädstruktur, ett så kallat Document Object Model (DOM), som sedan kan traverseras och manipuleras med arrows. Just `hxt` är känt för sin flexibilitet och kraft, men kan ha en brant inlärningskurva jämfört med mer imperativa bibliotek.

## Se även:
- HXTs officiella dokumentation: [https://hackage.haskell.org/package/hxt](https://hackage.haskell.org/package/hxt)
- En guide till arrows i Haskell, som är hjälpsam för att förstå `hxt`: [https://www.haskell.org/arrows/](https://www.haskell.org/arrows/)
- `tagsoup` för parsing av real-world HTML som inte är välformad: [https://hackage.haskell.org/package/tagsoup](https://hackage.haskell.org/package/tagsoup)
- `pandoc` för dokumentkonvertering med stark HTML-stöd: [https://hackage.haskell.org/package/pandoc](https://hackage.haskell.org/package/pandoc)
