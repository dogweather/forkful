---
title:                "Gleam: Slette tegn som matcher et mønster"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte behov for å slette bestemte tegn eller bokstaver fra en tekststreng når du arbeider med programmering. Dette kan være for å rense og formatere data, filtrere uønskede tegn eller bare for å forenkle tekstbehandlingen din. I denne bloggposten vil jeg vise deg hvordan du kan bruke Gleam til å slette tegn som matcher et bestemt mønster, og hvorfor dette kan være nyttig.

## Hvordan gjøre det

For å slette tegn som matcher et mønster i Gleam, kan du bruke Funktsjonen `String.replace` sammen med en regulær uttrykksparameter. La oss si at vi har en tekststreng som inneholder både tall og bokstaver, men vi kun ønsker å beholde bokstaver. Vi kan enkelt gjøre dette ved å bruke følgende kode:

```
Gleam
let tekst = "123-abc-456"
let kun_bokstaver = String.replace(tekst, ~regexp="\d", ~replacement="")
```

I dette eksempelet bruker vi funksjonen `String.replace` til å finne alle tall i tekststrengen og bytte dem ut med et tomt streng (""), altså slettet dem. Dette etterlater oss med tekststrengen "abc", som kun inneholder bokstaver.

I tillegg til å slette tegn, kan du også bruke denne metoden til å beholde kun visse tegn. For eksempel kan vi bruke følgende kode for å slette alle bokstaver fra en tekststreng, og kun beholde tallene:

```
Gleam
let tekst = "123-abc-456"
let kun_tall = String.replace(tekst, ~regexp="[a-zA-Z]", ~replacement="")
```

Dette vil gi oss tekststrengen "123-456".

## Dykk dypere

Nå som du har en forståelse for hvordan du kan slette tegn som matcher et mønster, kan du også utforske andre måter å bruke denne funksjonen på. For eksempel kan du kombinere `String.replace` med andre funksjoner for å manipulere og formatere tekst på en mer kompleks måte. Du kan også eksperimentere med ulike regulære uttrykk for å finne den perfekte strategien for ditt brukstilfelle.

## Se også

- [Gleam dokumentasjon om String modulet](https://gleam.run/documentation/std_lib/string/)
- [Essential Guide to Regular Expressions in Gleam](https://medium.com/@gleamlang/an-essential-guide-to-regular-expressions-in-gleam-72f24c1320f8)