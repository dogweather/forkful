---
title:                "Uthenting av delstrenger"
date:                  2024-01-20T17:46:04.790649-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente ut delstrenger betyr å plukke ut spesifikke deler av en tekststreng. Programmerere gjør dette for å manipulere, analysere eller transformere data på en mer finjustert måte.

## Slik gjør du:

I Haskell, brukes ofte `take`, `drop`, og `substring` funksjoner fra `Data.Text` biblioteket for å håndtere strenger.

```Haskell
import Data.Text as T

main :: IO ()
main = do
  let tekst = T.pack "Hei, Haskell!"
  let delstreng = T.take 3 tekst
  putStrLn $ T.unpack delstreng -- Output: "Hei"

  let hoppOver = T.drop 5 tekst
  putStrLn $ T.unpack hoppOver -- Output: "Haskell!"

  let substr = T.take 7 (T.drop 5 tekst)
  putStrLn $ T.unpack substr -- Output: "Haskell"
```

## Dypdykk

Før `Data.Text`, Haskell brukte `String` (som egentlig er en liste av `Char`s) for tekstbehandling. Men, å jobbe med `String` var tregt og ineffektivt for store tekster. Derfor ble `Data.Text` introdusert for å gi en mer effektiv strengbehandling.

Det finnes også biblioteket `text` som tilbyr funksjonen `takeEnd` og `dropEnd`, for å hente ut eller fjerne tegn fra slutten av tekststrenger.

Implementasjonsmessig, `Data.Text` bruker utf-16 koding intern, som er en balanse mellom effektivitet (sammenlignet med utf-32) og kompakthet (sammenlignet med utf-8 for visse språk).

## Se Også

- `Data.Text` dokumentasjon: https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html
- Et Haskell-kurs for de som vil lære mer: http://learnyouahaskell.com/chapters
- Haskell offisielle side: https://www.haskell.org/