---
date: 2024-01-20 17:42:22.629361-07:00
description: "I Haskell handler sletting av tegn som matcher et m\xF8nster om \xE5\
  \ fjerne spesifikke tegn fra en tekststreng basert p\xE5 gitte kriterier. Programmerere\
  \ gj\xF8r\u2026"
lastmod: '2024-03-13T22:44:40.824902-06:00'
model: gpt-4-1106-preview
summary: "I Haskell handler sletting av tegn som matcher et m\xF8nster om \xE5 fjerne\
  \ spesifikke tegn fra en tekststreng basert p\xE5 gitte kriterier."
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## How to:
I Haskell kan vi bruke `Data.Text` biblioteket til å arbeide med tekst og `Data.Text` sin funksjon `filter` for å slette tegn. Her er et eksempel:

```haskell
import Data.Text (Text, filter)

deletePattern :: Char -> Text -> Text
deletePattern pattern text = filter (/= pattern) text

main :: IO ()
main = do
    let text = "Heisann! Dette er en test."
    let result = deletePattern 'e' text
    print result
```

Kjøring av dette gir følgende resultat:

```
"Hisan! Dtt r n tst."
```

## Deep Dive
Sletting av mønstrende tegn er ikke nytt i Haskell, men `filter` funksjonen har eksistert i funksjonell programmering siden Lisp. Alternativer inkluderer bruk av regulære uttrykk med `Text.Regex` biblioteket, som kan være kraftigere, men også mer kompleks.

Implementasjonsdetaljer for `filter` er enkle: funksjonen traverserer gjennom tekststrengen, beholder tegn som ikke samsvarer med mønsteret og forkaster de som gjør det. Det er effektivt og lett å forstå.

## See Also
- Haskell `Text` pakken: https://hackage.haskell.org/package/text
- Regulære uttrykk i Haskell: https://hackage.haskell.org/package/regex-base

Her er også et godt eksempel på bruk av `Data.Text` for tekstmanipulering i Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation
