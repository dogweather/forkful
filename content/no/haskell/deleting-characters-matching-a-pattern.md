---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "Haskell: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sletting av tegn som matcher et mønster er en vanlig programmeringsoppgave hvor man fjerner bestemte tegn fra en streng basert på et gitt mønster. Dette er nyttig for å behandle og manipulere tekstbasert data, og er en viktig ferdighet for mange programmerere.

## Hvordan:

```Haskell
-- Eksempel 1: Fjerning av alle mellomrom fra en streng
deleteWhitespace :: String -> String
deleteWhitespace [] = []
deleteWhitespace (x:xs)
  | x == ' ' = deleteWhitespace xs
  | otherwise = x : deleteWhitespace xs

-- Kjøre kode
deleteWhitespace "Dette er en streng med mellomrom." 
-- Output: "Detteerenstrengmedmellomrom."

-- Eksempel 2: Fjerning av alle sifre fra en streng
deleteDigits :: String -> String
deleteDigits [] = []
deleteDigits (x:xs)
  | isDigit x = deleteDigits xs
  | otherwise = x : deleteDigits xs
 
-- Kjøre kode
deleteDigits "Dette er tallet 1234" 
-- Output: "Dette er tallet "

-- Eksempel 3: Fjerning av et bestemt tegn fra en streng
deleteChar :: Char -> String -> String
deleteChar c [] = []
deleteChar c (x:xs)
  | x == c = deleteChar c xs
  | otherwise = x : deleteChar c xs
 
-- Kjøre kode
deleteChar 'a' "Dette er bare en test" 
-- Output: "Dette er bre en test"

```

## Dykk dypere:
Sletting av tegn som matcher et mønster har vært en vanlig oppgave for programmerere siden begynnelsen av tekstbasert databehandling. Det finnes også alternative metoder for å gjøre dette, som for eksempel å bruke regulære uttrykk. I Haskell er det flere innebygde funksjoner som kan hjelpe til med sletting av tegn, som `filter`, `delete` og `strip`.

Når du skal implementere sletting av tegn som matcher et mønster i Haskell, er det viktig å ha et godt grep om lister og pattern matching. Det er også viktig å forstå hvordan funksjoner i Haskell fungerer og hvordan de kan kombineres for å oppnå ønsket resultat.

## Se også:
For mer informasjon om sletting av tegn som matcher et mønster i Haskell, sjekk ut disse kildene:

- [Haskell documentation on functions for working with lists](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html)
- [Video tutorial on pattern matching in Haskell](https://youtu.be/L8ae_7BkkW8)
- [Haskell documentation on pattern matching](https://wiki.haskell.org/Pattern_matching)