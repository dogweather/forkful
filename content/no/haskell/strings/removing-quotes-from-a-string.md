---
title:                "Fjerne anførselstegn fra en streng"
aliases:
- no/haskell/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:51.475870-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng betyr å stripe ut eventuelle sitattegn—enten enkle (' ') eller doble (" ")—som er en del av strengdataene. Programmerere gjør dette for å rense inndata, forberede tekst for behandling, eller bli kvitt unødvendige tegn som kan forstyrre håndtering av data og operasjoner.

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
