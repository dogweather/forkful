---
title:                "Elm: Søking og utskifting av tekst"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med programmering, er det alltid viktig å ha en effektiv og nøyaktig måte å endre tekst på. Det kan være å bytte ut enkelte ord, hele linjer eller til og med hele blokker med kode. Så hvordan gjør man dette i Elm? Det er akkurat det vi skal se på i denne bloggposten.

## Hvordan gjøre det

Så la oss komme i gang med å se på eksempler på hvordan man kan søke og erstatte tekst i Elm. Først trenger vi en tekst som vi kan jobbe med. La oss si vi har følgende tekststreng i vår `main` funksjon:

```Elm
main =
    let
        tekst = "Dette er en tekst som vi vil gjøre endringer på"
    in
    Html.text tekst
```

Vi vil nå bytte ut ordet "endringer" med "endringer og forbedringer". Dette kan vi enkelt gjøre ved å bruke funksjonen `String.replace` og gi den vår tekststreng, det gamle ordet og det nye ordet som argumenter. Vi kan deretter vise den nye teksten ved å bruke `Html.text` funksjonen igjen. Se koden nedenfor:

```Elm
main =
    let
        tekst = "Dette er en tekst som vi vil gjøre endringer på"
        nyTekst = String.replace "endringer" "endringer og forbedringer" tekst
    in
    Html.text nyTekst
```

Kjører vi denne koden, vil vi få følgende output:

```Elm
Dette er en tekst som vi vil gjøre endringer og forbedringer på
```

Som du kan se, vil det gamle ordet bli erstattet med det nye. Men hva hvis vi ønsker å gjøre flere endringer i samme tekststreng? Da kan vi bruke funksjonen `String.map` som lar oss gå gjennom tekststrengen og gjøre en endring på hvert enkelt tegn. Se koden nedenfor:

```Elm
main =
    let
        tekst = "Dette er en tekst som vi vil gjøre endringer på"
        nyTekst = String.map (\c -> if c == 'e' then '9' else c) tekst
    in
    Html.text nyTekst
```

I dette eksemplet erstatter vi alle forekomster av bokstaven "e" med tallet 9. Kjører vi koden, vil vi få følgende output:

```Elm
D9tt9 9r 9n t9kst som vi vil gjør9 9ndring9r på
```

## Dypdykk

Nå som vi har sett på noen enkle eksempler, la oss ta et dypere dykk og se på noen flere funksjoner som kan være nyttige når man skal søke og erstatte tekst i Elm.

En annen viktig funksjon er `String.explode` som lar oss dele opp en tekststreng i en liste av tegn. Dette kan være nyttig hvis vi for eksempel ønsker å utføre en handling på hvert enkelt ord i en tekst. Se koden nedenfor:

```Elm
main =
    let
        tekst = "Dette er en tekst som vi vil gjøre endringer på"
        ordListe = String.explode " " tekst
        nyTekst = String.join " " (List.map (\ord -> String.toUpper ord) ordListe)
    in
    Html.text nyTekst
```

I dette eksemplet deler vi opp tekststrengen etter mellomrom og gjør alle ordene store bokstaver ved å bruke funksjonen `String.toUpper`. Deretter setter vi ordene sammen igjen ved å bruke `String.join`. Kjører vi koden, vil vi få følgende output:

```Elm
DETTE ER EN TEKST SOM VI VIL GJØRE ENDRINGER PÅ
```

En annen nyttig funksjon når man arbeider med tekst, er `String.slice` som lar oss hente ut en del av en tekststreng basert på en start- og sluttposisjon. Se koden nedenfor:

```Elm
main =
    let
        randomTek