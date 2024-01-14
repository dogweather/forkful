---
title:    "Elm: Sammenslåing av strenger"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor
Å samle strenger, også kjent som å slå sammen eller konkatenering, er en viktig del av programmering. Det lar deg kombinere forskjellige tekststrenger for å lage mer komplekse og dynamiske utdata. I denne bloggposten skal vi utforske hvordan man kan gjøre dette ved hjelp av Elm-programmeringsspråket.

## Hvordan
For å konkatener tekststrenger i Elm, kan du bruke operatoren `++`. Denne operatoren kombinerer to strenger og gir som resultat en ny streng. La oss se på et eksempel:

```Elm
str1 = "Hei"
str2 = "verden!"
combined = str1 ++ " " ++ str2
```

I dette eksempelet kombinerer vi strengene "Hei" og "verden!" ved hjelp av `++` operatoren. Den siste linjen vil da resultere i "Hei verden!" som utdata. Du kan også bruke denne operatoren til å kombinere flere strenger på en gang:

```Elm
str1 = "Hvilken"
str2 = "dag"
str3 = "er"
str4 = "det?"
combined = str1 ++ " " ++ str2 ++ " " ++ str3 ++ " " ++ str4
```

Denne koden vil gi oss utdataen "Hvilken dag er det?".

## Dypdykk
I tillegg til å kombinere vanlige strenger, kan du også bruke `++` operatoren til å konkatenerere tall og andre datatyper i Elm. For eksempel:

```Elm
str1 = "Jeg er"
age = 27
str2 = "år gammel"
combined = str1 ++ " " ++ (toString age) ++ " " ++ str2
```

Denne koden vil konkatenerere strengen "Jeg er" og "år gammel" med verdien av variabelen `age`, som i dette tilfellet er 27. Vi bruker funksjonen `toString` for å konvertere tallet til en streng, slik at det kan slås sammen med de andre strengene.

## Se Også
- [Offisiell Elm Dokumentasjon](https://elm-lang.org/docs)
- [Elm kurs for nybegynnere](https://egghead.io/browse/frameworks/elm)
- [Elm fellesskapet på Reddit](https://www.reddit.com/r/elm/)