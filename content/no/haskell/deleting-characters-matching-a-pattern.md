---
title:    "Haskell: Sletting av tegn som matcher et mønster."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være nyttig når man trenger å rydde opp i tekstfiler eller datasett. Ved å fjerne unødvendige tegn, kan man gjøre tekstbehandling mer effektiv og strukturere data på en mer oversiktlig måte.

## Hvordan

Det finnes flere måter å slette tegn som matcher et mønster i Haskell på. Her er et eksempel på en enkel funksjon som fjerner alle forekomster av et bestemt tegn i en streng:

```Haskell
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar ch (x:xs) | ch == x = removeChar ch xs
                     | otherwise = x : removeChar ch xs
```

La oss nå teste funksjonen ved å kjøre følgende kommando i GHCi:

```Haskell
removeChar 'a' "Hello, world!"
```

Dette vil gi oss følgende output:

```
"Hello, world!"
```

Som man kan se, har alle bokstavene "a" blitt fjernet fra strengen. Man kan også bruke denne funksjonen til å fjerne flere typer tegn ved å gjenta kommandoen med ulike tegn som argument.

## Dypdykk

For å forstå hvordan funksjonen vår fungerer, kan det være nyttig å se nærmere på mønstermatching i Haskell. I denne konteksten vil "mønster" referere til en sekvens av elementer, for eksempel en streng av tegn. Mønstermatching lar oss sammenligne en verdi med et gitt mønster og gjøre en handling basert på om verdiene matcher eller ikke.

I vårt tilfelle bruker vi mønstermatching til å sjekke om tegnet vi ønsker å fjerne (representert ved variabelen "ch") er lik det nåværende tegnet som blir undersøkt (representert ved variabelen "x"). Hvis det er tilfelle, går vi videre til å undersøke det neste tegnet i strengen. Hvis ikke, legger vi til det nåværende tegnet i den nye strengen som blir returnert.

Dette er bare én av mange måter å slette tegn som matcher et mønster i Haskell på. Det finnes også moduler og funksjoner som er spesielt utviklet for tekstbehandling og mønstermatching, som kan være nyttige når man arbeider med mer komplekse datasett.

## Se også

- [Haskell.org](https://www.haskell.org/) - offisiell nettside for Haskell
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - en interaktiv guide til Haskell
- [Data.ByteString.Char8](https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString-Char8.html) - en modul for håndtering av tegn i bytestrings i Haskell