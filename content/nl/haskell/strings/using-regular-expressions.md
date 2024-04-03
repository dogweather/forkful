---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:28.945295-07:00
description: 'Hoe te gebruiken: In Haskell kun je regex gebruiken met het `regex-tdfa`
  pakket. Hier pakken we getallen uit een tekst.'
lastmod: '2024-03-13T22:44:50.842220-06:00'
model: gpt-4-0125-preview
summary: In Haskell kun je regex gebruiken met het `regex-tdfa` pakket.
title: Reguliere expressies gebruiken
weight: 11
---

## Hoe te gebruiken:
In Haskell kun je regex gebruiken met het `regex-tdfa` pakket. Hier pakken we getallen uit een tekst.

```Haskell
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  let text = "Order 531 heeft 2 items"
  let numbers = text =~ "[0-9]+" :: [String]
  print numbers
```

Output:
```
["531","2"]
```

Om tekst te vervangen kun je `subRegex` van `regex-compat` gebruiken.

```Haskell
import Text.Regex (subRegex, mkRegex)

main :: IO ()
main = do
  let text = "Hallo, 2023!"
  let regex = mkRegex "[0-9]+"
  let newText = subRegex regex text "JAAR"
  putStrLn newText
```

Output:
```
Hallo, JAAR!
```

## Diepgaand
Reguliere expressies dateren uit de jaren 1950, geconceptualiseerd door wiskundige Stephen Kleene. Hoewel Haskell later in het spel was, heeft het nu een rijke verzameling van regex-bibliotheken zoals `regex-tdfa` voor POSIX regex, en `regex-pcre` voor Perl-compatibiliteit. Alternatieven voor regex zijn parser combinator bibliotheken zoals `parsec`, die meer leesbaarheid en onderhoudbaarheid kunnen bieden. Regex's in Haskell zijn niet ingebouwd in de syntaxis van de taal, maar worden aangeboden via deze bibliotheken.

## Zie ook
- Hackage bibliotheken:
  - regex-tdfa: http://hackage.haskell.org/package/regex-tdfa
  - regex-compat: http://hackage.haskell.org/package/regex-compat
  - regex-pcre: http://hackage.haskell.org/package/regex-pcre
- De Haskell Wiki over reguliere expressies: https://wiki.haskell.org/Regular_expressions
- "Real World Haskell" door Bryan O'Sullivan, Don Stewart, en John Goerzen voor een diepgaande behandeling: http://book.realworldhaskell.org/
