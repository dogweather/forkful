---
title:                "Håndtering av HTML."
html_title:           "Haskell: Håndtering av HTML."
simple_title:         "Håndtering av HTML."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Parsing HTML er prosessen med å analysere og tolke HTML-kode for å kunne hente ut informasjon fra nettsider. Dette er nyttig for programmerere som trenger å skrive kode som kan behandle og manipulere nettsideinnhold.

# Hvordan:
Her er en enkel funksjon i Haskell som bruker et innebygd bibliotek kalt "tagsoup" for å utføre parsing av en nettside. Denne funksjonen tar inn en URL som argument og returnerer en liste over alle tekstblokkene på nettsiden.

```Haskell
import Text.HTML.TagSoup

parseHTML :: String -> IO [String]
parseHTML url = do
    html <- getResponseBody =<< simpleHTTP (getRequest url)
    return $ map (innerText . canonizeTags) $ partitions (~== "<p>") $ parseTags html
```

Eksempel på bruk av funksjonen:

```Haskell
main :: IO ()
main = do
    articles <- parseHTML "https://www.example.com/articles"
    mapM_ putStrLn articles
```

Eksempel på output:

```
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus id faucibus leo, non pharetra erat.
Sed vel ligula eget ex mollis mattis.
```

# Dykke dypere:
Parsing av HTML har en interessant historie, da HTML standarden har utviklet seg over tid og har ført til forskjellige implementeringer og tilnærminger for parsing. Noen programmerere bruker andre språk som for eksempel Python eller JavaScript for å gjøre dette, men med Haskell og bibliotek som tagsoup er det enkelt å utføre parsing også.

# Se også:
- [Tagsoup biblioteket](https://hackage.haskell.org/package/tagsoup)
- [Dypdykk i HTML parsing med Haskell](https://www.fpcomplete.com/blog/2017/04/html-parsing-with-tagsoup)