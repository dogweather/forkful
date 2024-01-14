---
title:    "Haskell: Finne lengden på en streng"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor
Når vi jobber med programmering, er det ofte viktig å kunne finne lengden på en streng. Dette kan være nyttig når vi skal manipulere tekst eller utføre dataanalyse. I denne bloggposten skal vi utforske hvordan vi kan finne lengden på en streng ved hjelp av Haskell.

## Hvordan
For å begynne, må vi først definere en variabel som inneholder en streng. La oss kalle den "tekst" og gi den verdien "Hei, verden!". Vi kan da bruke funksjonen "length" for å finne lengden på denne strengen. I Haskell ser det slik ut:

```Haskell
tekst = "Hei, verden!"
length tekst
```

Dette vil gi oss outputen "13", som er lengden på strengen vår. La oss se på et annet eksempel, der vi ønsker å finne lengden på en streng som inneholder et navn:

```Haskell
navn = "Marie"
length navn
```

Her vil outputen være "5", siden navnet "Marie" består av fem bokstaver. Vi kan også kombinere "length" med andre funksjoner, for eksempel "head", som lar oss ta ut den første bokstaven i en streng. La oss se et eksempel på det:

```Haskell
navn = "Marie"
head navn
length (head navn)
```

Dette vil gi oss outputen "M" og "1", siden "M" er den første bokstaven i "Marie".

## Dypdykk
Nå som vi har sett noen eksempler på hvordan vi kan finne lengden på en streng i Haskell, la oss se litt nærmere på hvordan dette egentlig fungerer. I Haskell er en streng en liste av karakterer. Når vi bruker funksjonen "length", teller den simpelthen antall elementer i denne listen. Dette betyr at enhver streng, uavhengig av hva den inneholder, kan finne sin lengde på denne måten.

Vi kan også bruke "length" på lister av tall, og den vil da gi oss antall elementer i listen. Dette gjør funksjonen veldig allsidig og nyttig i mange ulike situasjoner.

## Se også
- [Offisiell Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Codecademy kurs i Haskell](https://www.codecademy.com/learn/learn-haskell)