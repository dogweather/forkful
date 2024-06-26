---
date: 2024-01-26 03:39:51.475870-07:00
description: "Hvordan: I Haskell kan vi lage en funksjon som fjerner alle anf\xF8\
  rselstegn fra en gitt streng. Det er som \xE5 fortelle anf\xF8rselstegnene \xE5\
  \ stikke av, og s\xF8rge\u2026"
lastmod: '2024-03-13T22:44:40.828925-06:00'
model: gpt-4-0125-preview
summary: "I Haskell kan vi lage en funksjon som fjerner alle anf\xF8rselstegn fra\
  \ en gitt streng."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
I Haskell kan vi lage en funksjon som fjerner alle anførselstegn fra en gitt streng. Det er som å fortelle anførselstegnene å stikke av, og sørge for at de tar hintet.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell sa, \"La oss lære noen funksjoner!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Eksempel på utdata:

```
Haskell sa, La oss lære noen funksjoner!
```

## Dypdykk
Det var en gang, før strenger i programmering var like vanlig som kattevideoer på internett, var tekstbehandling en finurlig virksomhet. Men ettersom programmeringsspråk utviklet seg, ble strenger en avgjørende del av kodingen. Likevel forble anførselstegn et tveegget sverd—essensielt for å definere strenger, men et bryderi når de ble inkludert som faktiske data.

Alternativer? I stedet for å swatte vekk alle anførselstegn som fluer, kan du være selektiv. Du vil kanskje bare fjerne de ytterste anførselstegnene (en klassisk trim) eller håndtere escapede anførselstegn inni en streng.

Når det gjelder implementasjon, bruker `removeQuotes`-funksjonen over et lambda til å kontrollere hver karakter (`c`) for å se om det er et plagsomt anførselstegn og filtrerer dem ut deretter. Dette er en grei tilnærming, men for større tekster eller mer komplekse regler, kan du ønske å se på parserbiblioteker som `Parsec` som kan gi deg mer finesse og kraft i tekstbehandlingen.

## Se også:
- For regex-elskere: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- En vennlig introduksjon til Haskell-strenger: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
