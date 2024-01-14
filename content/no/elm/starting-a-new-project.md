---
title:                "Elm: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt program kan virke overveldende og skummelt, spesielt hvis du ikke er kjent med programmering. Men å hoppe inn i et nytt språk som Elm kan være en spennende og givende opplevelse. Med sine funksjonelle og feilfrie egenskaper, kan Elm hjelpe deg med å lage robuste og pålitelige programmer.

## Hvordan

For å sette i gang med Elm, må du først installere Elm-plattformen på datamaskinen din. Deretter er det en god ide å følge en online-opplæring eller lese en bok for å få en grunnleggende forståelse av språket. Når du har grunnlaget, kan du begynne å skrive kode.

```elm
-- Dette er et eksempel på en funksjon som legger sammen to tall og returnerer summen.
sum : Int -> Int -> Int
sum x y =
  x + y
```

En ting som gjør Elm spesielt er dens evne til å håndtere feil på en elegant måte. I koden ovenfor, hvis vi prøver å legge sammen to strenger i stedet for tall, vil Elm advare oss og forhindre at koden vår kjører med feil data.

## Deep Dive

Når du begynner på din første Elm-applikasjon, er det viktig å vite hvordan du skal strukturere og organisere koden din. Elm har et modulsystem som hjelper deg å dele opp koden din i separate deler, og dette gir en mer robust og modulær kodebase.

En annen viktig del av å starte et nytt prosjekt i Elm er å forstå hvordan man håndterer sideeffekter. I motsetning til andre språk, der sideeffekter kan være en kilde til feil, har Elm en dedikert arkitektur for å håndtere disse effektene på en trygg måte.

## Se også

- [Elm sin offisielle hjemmeside](https://elm-lang.org/)
- [Elm guide på norsk](https://skogman.github.io/)
- [Online Elm-kurs](https://egghead.io/courses/start-using-elm-to-build-web-applications)