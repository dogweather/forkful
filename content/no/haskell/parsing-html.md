---
title:                "Analysering av html"
html_title:           "Haskell: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal man engasjere seg i å parse HTML? Vel, hvis du jobber med webutvikling eller programmering, vil du mest sannsynlig komme over HTML-kode som skal leses og behandles. Å kunne parse, eller "analysere" i uformell terminologi, HTML er en viktig ferdighet for å kunne jobbe med webinnhold.

## Slik gjør du det

Du kan parse HTML ved hjelp av forskjellige programmeringsspråk, men vi skal fokusere på å gjøre det med Haskell. Først og fremst trenger du et åpnbart bibliotek for å arbeide med HTML, for eksempel "tagsoup" biblioteket. La oss si at du har en HTML fil med følgende innhold:

```Haskell
"<html>
    <body>
        <h1>Hei, verden!</h1>
        <p>Dette er en test</p>
    </body>
</html>"
```

Vi kan da bruke "tagsoup" biblioteket til å parse den og hente ut overskriften og teksten:

```Haskell
-- Importer tagsoup biblioteket
import Text.HTML.TagSoup

-- Definer HTML-koden
htmlCode :: String
htmlCode = "<html>
                <body>
                    <h1>Hei, verden!</h1>
                    <p>Dette er en test</p>
                </body>
            </html>"

-- Parse HTML-koden og hente ut overskriften og teksten
parsedCode :: [Tag String]
parsedCode = parseTags htmlCode

heading :: String
heading = fromTagText $ head $ dropWhile (~/= "<h1>") parsedCode

text :: String
text = fromTagText $ head $ dropWhile (~/= "<p>") $ tail $ dropWhile (~/= "<h1>") parsedCode

-- Skriv ut resultat
main :: IO ()
main = do
    putStrLn heading   -- "Hei, verden!"
    putStrLn text      -- "Dette er en test"
```

Som du kan se, kan du enkelt hente ut spesifikke deler av HTML-koden ved hjelp av "parseTags" funksjonen og deretter bruke vanlig Haskell-kode for å behandle og skrive ut informasjonen du ønsker.

## Dybdeanalyse

Når du arbeider med å parse HTML, er det viktig å forstå hvordan tags og attributter fungerer. Tags er elementene i HTML som definerer innholdet på en nettside, for eksempel <h1>, <p>, <a>, etc. Disse tagsene kan også ha attributter, som gir ytterligere informasjon om taggen, for eksempel "class", "id", "href", og så videre. Å forstå strukturen til HTML-koden og hvordan disse tagsene og attributtene fungerer, er avgjørende for å kunne parse den nøyaktig.

## Se også

- [Offisiell Haskell hjemmeside](https://www.haskell.org/)
- [Tagsoup biblioteket](https://github.com/ndmitchell/tagsoup)
- [HTML tutorial fra W3Schools](https://www.w3schools.com/html/)