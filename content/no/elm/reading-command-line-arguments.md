---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kommandolinje argumenter er innspill som blir gitt til et program når det kjøres, som brukes til å tilpasse programmets oppførsel. Programmerere bruker det for å gjøre programmer fleksible og enkle å tilpasse forskjellige bruksområder.

## Hvordan å:

Elm (nåværende versjon) støtter dessverre ikke kommandolinje-argumenter direkte. Men du kan bruke JavaScript interopp for å lese argumentene i stedet.

```Elm
port module Main exposing (..)

port sendFlags : List String -> Cmd msg
```

Ovenstående kodeeksempel definerer en interfond til JavaScript-verdenen for å sende argumenter som en liste med strenger.

Du ville deretter kalle "sendFlags" fra JavaScript, noe slik:

```JavaScript
const app = Elm.Main.init();

app.ports.sendFlags.send(process.argv);
```

## Dypere Dykk

Historisk sett har ikke Elm-støtte for kommandolinje-argumenter vært et prioritert trekk. Dette skyldes hovedsakelig at Elm er ment for frontend-utvikling, hvor kommandolinje-argumenter er mindre relevante.

Som et alternativ kan programmerere bruke Elm sin interop-funksjonalitet for å koble til JavaScript, som har full støtte for kommandolinje-argumenter.

Selv om det er mulig å bruke kommandolinje-argumenter i Elm via JavaScript, er det viktig å merke seg at dette kan føre til en mer kompleks kodebase. Dette skyldes at man må administrere kommunikasjonen mellom Elm og JavaScript, og også ta hånd om eventuelle feil og unntak som kan oppstå i prosessen.

## Se Også:

- Elm dokumentasjon: samsyn med JavaScript: https://guide.elm-lang.org/interop/
- Forstå Javascript og Node.js kommandolinje-argumenter: https://flaviocopes.com/node-command-line-args/
- Alternativer for å klare argumenter i JavaScript: https://www.npmjs.com/package/commander