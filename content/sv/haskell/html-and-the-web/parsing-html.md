---
date: 2024-01-20 15:31:59.808672-07:00
description: "Hur g\xF6r man: F\xF6r att parsa HTML i Haskell kan vi anv\xE4nda sig\
  \ av biblioteket `hxt`, som st\xE5r f\xF6r Haskell XML Toolbox. H\xE4r \xE4r en\
  \ snabb uppvisning."
lastmod: '2024-03-13T22:44:37.952895-06:00'
model: unknown
summary: "F\xF6r att parsa HTML i Haskell kan vi anv\xE4nda sig av biblioteket `hxt`,\
  \ som st\xE5r f\xF6r Haskell XML Toolbox."
title: Tolka HTML
weight: 43
---

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
