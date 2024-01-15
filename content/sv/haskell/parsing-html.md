---
title:                "Analys av html"
html_title:           "Haskell: Analys av html"
simple_title:         "Analys av html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle någon vilja syssla med att parse:a HTML? En stor anledning är för att kunna extrahera data från webbsidor och använda den i sina program eller analyser. Till exempel kan man hämta information från en online-butik och använda den för att jämföra priser eller skapa ett personligt prisövervakningsprogram.

## Så här gör du
Att parse:a HTML i Haskell är enkelt med hjälp av paketet "tagsoup". Först måste du installera paketet genom att skriva ```cabal install tagsoup```. Sedan kan du importera biblioteket och använda funktionen "parseTags" för att konvertera HTML-koden till en taglista.

```haskell
import Text.HTML.TagSoup

main = do
  html <- readFile "example.html"
  let tags = parseTags html
  print tags
```

Detta kommer att skriva ut en taglista som representerar strukturen av HTML-dokumentet. Du kan sedan använda olika funktioner för att filtrera och bearbeta taglistan för att extrahera den data du behöver.

```haskell
import Text.HTML.TagSoup

main = do
  html <- readFile "example.html"
  let tags = parseTags html
  let links = filter (\x -> isTagOpenName "a" x) tags
  let urls = map (fromAttrib "href") links
  print urls
```

I detta exempel hämtar vi alla "a"-taggar och extraherar länkadresserna. Slutligen skriver vi ut dem för att se resultaten.

## Djupdykning
TagSoup erbjuder också många andra användbara funktioner för att hantera HTML, som till exempel att konstruera taggar, modifiera attribut och ta bort taggar. Det finns också andra paket som kan vara användbara för mer specialiserade parsinguppgifter, som "html-conduit" för att använda strängt effektfulla parseringar.

## Se även
- Haskells officiella dokumentation för tagSoup: <https://hackage.haskell.org/package/tagsoup/docs/Text-HTML-TagSoup.html>
- "html-conduit"-paketet: <https://hackage.haskell.org/package/html-conduit>
- Ett exempelprojekt som använder TagSoup: <https://github.com/lpsmith/tagSoup-example>