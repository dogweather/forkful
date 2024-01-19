---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konkatenering av strenger er prosessen med å kombinere to eller flere strenger sammen til en. Programmerere gjør dette å bygge nye setninger, meldinger, eller verdier fra eksisterende informasjon.

## Hvordan:

I Haskell, er det flere måter å konkatnere strengs på. La oss se på noen eksempler:

```Haskell
-- Bruk av (++)
greetings = "Hei, " ++ "Hvordan går det?"
-- Output: "Hei, Hvordan går det?"

-- Bruk av unwords
fruits = unwords ["Epler", "Bananer", "Appelsiner"]
-- Output: "Epler Bananer Appelsiner"
  
-- Bruk av concat
numList = concat [show x | x <- [1..5]]
-- Output: "12345"
```

## Dypdykk

Historisk sett, datamaskiner behandlet tekst som sekvenser av tegn. Konkatenering var og er fortsatt en grunnleggende operasjon for tekstbehandling. I Haskell, kan "++" eller "concat" operatoren brukes. "++" er mer naturlig for mennesker å lese, men "concat" kan være mer effektiv for store lister.

Et alternativ til å bruke Haskell for strengkonkatenering er å bruke en annen programmeringsspråk som støtter denne operasjonen, som Python eller JavaScript.

Implementeringen av konkatenering kan variere avhengig av språkgrensesnittet. Men i Haskell, "++" og "concat" operatører er standardmetoder for å oppnå dette.

## Se Også 

Relaterte kilder for å lærer mer om Haskell og strimg konkatenering:

- Haskell Wiki om Strings: https://wiki.haskell.org/Strings
- Lær deg Haskell (Norsk): https://www.haskell.no/laer-deg-haskell
- Real World Haskell: http://book.realworldhaskell.org/