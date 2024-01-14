---
title:                "Haskell: Parse html"
simple_title:         "Parse html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att behandla HTML är en viktig del av webbutveckling. Genom att kunna parsa HTML kan man extrahera och manipulera data från webbsidor, vilket kan vara användbart för uppgifter som webbskrapning eller webbautomatisering.

## Så här gör du

För att parsy HTML i Haskell finns det flera bibliotek som kan användas, till exempel "html-conduit" och "tagsoup". Vi kommer att använda det senare i vårt exempel.

Först måste vi importera biblioteket och skapa en HTTP manager:

```Haskell
import Text.HTML.TagSoup
import Network.HTTP.Conduit

manager <- newManager tlsManagerSettings
```

Sedan kan vi hämta en webbsidas HTML-kod med hjälp av manageren och använda funktionen "parseTags" för att konvertera den till en lista med taggar:

```Haskell
request <- parseUrl "https://www.example.com"
response <- httpLbs request manager
let html = responseBody response
let tags = parseTags html
```

Nu kan vi göra en enkel sökning genom att använda "isTagOpen" för att hitta öppningstaggar med ett visst namn och sedan "fromAttrib" för att extrahera innehållet i dess attribut:

```Haskell
let links = filter (isTagOpenName "a") tags
let hrefs = map (fromAttrib "href") links
```

I vårt exempel har vi skapat en lista med länkar från webbsidan. Men det finns många andra funktioner och metoder som kan användas för att göra avancerade manipulationer av HTML-koden.

## Djupdykning

En viktig del av att kunna parsy HTML är att förstå dess struktur. HTML består av olika taggar som har ett namn och eventuellt attribut och kan även innehålla andra taggar, vilket skapar en hierarkisk struktur.

Med hjälp av HTML-parsers kan man navigera genom denna hierarki och extrahera önskad data. Det finns också metoder för att filtrera ut specifika taggar eller innehåll baserat på vissa kriterier.

Det är viktigt att vara försiktig när man använder en parser, eftersom felaktig HTML-kod kan orsaka problem. Därför är det alltid en bra idé att testa olika scenarier för att se hur din parser hanterar dem.

## Se även

- [Text.HTML.TagSoup dokumentation](https://hackage.haskell.org/package/tagsoup)
- [Thesis - Web scraping using Haskell](https://www.diku.dk/~andersg/thesis.pdf)
- [Haskell Web scraping tutorial](https://www.schoolofhaskell.com/user/scapinhaskell/web-scraping-in-haskell)