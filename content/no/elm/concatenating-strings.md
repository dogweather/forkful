---
title:                "Sammenkobling av strenger"
html_title:           "Elm: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor kombinere strenger? Fordi det kan gi en mer dynamisk og tilpasset tekst til forespørsler og brukerinteraksjoner i en applikasjon. Det å kombinere forskjellige variabler og tekststrenger i en kohesiv setning kan også gjøre koden mer lesbar og effektiv.

## Hvordan gjøre det

For å kombinere strenger i Elm, kan vi bruke funksjonen "++" (også kjent som "concat" eller "append"). Denne funksjonen tar inn to strenger og kombinerer dem til én. La oss se på et eksempel:

```Elm
concatStrings name age =
    "Hei, mitt navn er " ++ name ++ " og jeg er " ++ (String.fromInt age) ++ " år gammel."
```

I dette eksemplet tar vi inn en variabel for navn og en for alder. Ved å bruke ++-operatøren, kombinerer vi variablene og skaper en ny streng som inneholder både navn og alder.

**Output:**
```
Hei, mitt navn er Eric og jeg er 27 år gammel.
```

Vi kan også bruke ++-operatøren til å kombinere strenger med vanlige tekststrenger som ikke er variabler. La oss se på et annet eksempel:

```Elm
concatWithText color fruit =
    "Jeg elsker min " ++ color ++ " " ++ fruit ++ "."
```

I dette eksemplet tar vi inn variabler for farge og frukt og kombinerer dem med en fast tekststreng. Det er viktig å merke seg at hvis vi ikke bruker paranteser rundt den numeriske variabelen, vil ikke ++-operatøren fungere. Dette er fordi Elm trenger å vite hvilken del av uttrykket som skal evalueres som en streng og hvilken som skal evalueres som en numerisk verdi.

**Output:**
```
Jeg elsker min røde eple.
```

## Deep Dive

Det finnes også andre måter å kombinere strenger på i Elm, som å bruke built-in funksjoner som "concat", "append", eller å bruke lister og "foldl" funksjonen.

Det er også verdt å merke seg at i Elm er strenger uforanderlige, noe som betyr at når vi kombinerer strenger, oppretter vi faktisk en ny streng i stedet for å endre den eksisterende. Dette er en viktig designbeslutning i Elm og kan hjelpe til med å forebygge feil i koden.

## Se også

Ønsker du å lære mer om strenger i Elm? Sjekk ut disse ressursene:

- [Elm Offisiell Dokumentasjon](https://elm-lang.org/docs)
- [Liniker for å lære Elm](https://lineofcode.io/learn-elm)