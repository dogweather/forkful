---
title:    "Haskell: Å finne lengden på en streng"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng kan virke som en enkel oppgave, men når man begynner å jobbe med større programmeringsprosjekter, vil man ofte støte på oppgaver som krever denne kunnskapen. Å kunne finne lengden til en streng er en grunnleggende ferdighet som er nyttig i mange ulike programmeringsspråk, og kan hjelpe deg å skrive mer effektiv kode.

## Hvordan

```Haskell
-- Definerer en funksjon for å finne lengden til en streng
finnLengde :: String -> Int 
-- Bruker funksjonen "length" for å finne lengden til strengen
finnLengde myStreng = length myStreng 

-- Eksempel på bruk av funksjonen 
finnLengde "Dette er en streng" 
-- Output: 19
```

For å finne lengden til en streng i Haskell, kan man bruke den innebygde funksjonen "length". Denne tar inn en streng som parameter og returnerer antall tegn i strengen. I eksempelet over, tar vi inn strengen "Dette er en streng" og funksjonen returnerer verdien 19. Det er også mulig å definere sin egen funksjon for å finne lengden til en streng, som vist i koden over. Det kan være nyttig å bruke en egen funksjon for å finne lengden til en streng dersom man ønsker å implementere egne logikker eller utføre andre operasjoner på strengen før man finner lengden.

## Dypdykk

Når man jobber med større programmeringsprosjekter, kan det være nyttig å forstå hvordan lengden til en streng egentlig beregnes av datamaskinen. I Haskell og mange andre programmeringsspråk, blir tekst representer som en sekvens av Unicode-tegn. Hver av disse tegnene har en numerisk verdi som representerer dem. Når man finner lengden til en streng, blir hver av disse tegnene talt og returnert som resultat. Dette kan kompliseres ved for eksempel multibyte-strenger, hvor hvert tegn representeres av flere bytes, eller ved spesielle tegn som newline eller tab, hvor lengden kan variere avhengig av konteksten de brukes i. Det er derfor viktig å være bevisst på hvordan datamaskinen behandler og teller tekst for å kunne få korrekt lengde på en streng.

## Se Også
- [Offisiell Haskell dokumentasjon](https://www.haskell.org/documentation/) 
- [Haskell tutorial for nybegynnere](https://learnxinyminutes.com/docs/no-no/haskell/)