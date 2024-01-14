---
title:    "Go: Generering av tilfeldige tall"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en vanlig oppgave i mange programmeringsoppgaver. Enten det er for å lage et spill, teste algoritmer eller for å sikre sikkerheten i kryptografi, er muligheten til å generere tilfeldige tall essensiell. I denne bloggposten vil vi utforske hvordan vi kan generere tilfeldige tall i Go-programmeringsspråket.

## Hvordan
For å generere tilfeldige tall i Go, kan vi bruke pakken "math / rand". Først må vi importere denne pakken i koden vår ved å legge til "import (" math / rand ")" øverst i filen.

Deretter kan vi bruke "rand.Intn (n)" for å generere et tilfeldig tall mellom 0 og n-1. Hvis vi for eksempel vil generere et tilfeldig tall mellom 1 og 10, kan vi bruke "rand.Intn (10) + 1". Vi kan også bruke "rand.Float64 ()" for å generere et tilfeldig desimaltall mellom 0 og 1.

La oss se på et eksempel på hvordan dette kan se ut i praksis:

```Go
package main

import ("fmt"
        "math/rand")

func main () {
  num := rand.Intn (10) + 1
  fmt.Println ("Det tilfeldige tallet er:", num)
}
```
Output:
```
Det tilfeldige tallet er: 7
```

## Dypdykk
Å generere tilfeldige tall kan virke enkelt, men det er faktisk en kompleks oppgave. Det er fordi datamaskiner er deterministiske og dermed ikke i stand til å generere helt tilfeldige tall av seg selv. I stedet bruker de algoritmer som bruker en "seed" som starter punktet for å generere tallsekvensen.

I Go bruker "math / rand" pakken en algoritme som kalles Mersenne Twister for å generere tilfeldige tall. Denne algoritmen er designet for å være svært effektiv og har en stor periode, noe som betyr at den kan generere mange forskjellige tall før den gjentar sekvensen.

Det er også viktig å merke seg at datamaskiner ikke er i stand til å generere ekte tilfeldighet. Derfor bør tilfeldige tall som brukes i sikkerhetsrelaterte oppgaver, som kryptografi, hentes fra eksterne kilder som for eksempel en fysisk tilfeldig tallgenerator.

## Se Også
* [Offisiell Go-dokumentasjon om å generere tilfeldige tall](https://golang.org/pkg/math/rand/)
* [En artikkel om noen av de vanligste måtene å generere tilfeldige tall i programmering](https://www.baeldung.com/java-random)
* [En artikkel som diskuterer de begrensningene og risikoene ved generering av tilfeldige tall på datamaskiner](https://www.random.org/randomness/)