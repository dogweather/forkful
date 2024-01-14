---
title:    "Gleam: Utskrift av feilrettingsoutput"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å printe ut debug-utdata er et nyttig verktøy for å identifisere og feilsøke problemer i koden din. Dette kan spare deg for mye tid og frustrasjon i utviklingsprosessen.

## Hvordan

For å printe ut debug-utdata i Gleam, bruker du funksjonen "io:format". Her er et enkelt eksempel:

```Gleam
let min_variabel = "Gleam"
io:format("Variabelen min er: {}", [min_variabel])
```

Dette vil gi følgende utdata:

```
Variabelen min er: Gleam
```

Du kan også bruke "io:format" for å printe ut flere variabler og tekst ved å sette en kodesekvens for hver variabel i den andre argumentlisten, for eksempel:

```Gleam
let navn = "Maria"
let alder = 28
io:format("Hei, mitt navn er {} og jeg er {} år gammel.", [navn, alder])
```

Dette vil gi følgende utdata:

```
Hei, mitt navn er Maria og jeg er 28 år gammel.
```

## Dypdykk

Du kan også formatere utdataen ved å bruke spesifikke argumenter, for eksempel:

```Gleam
let nummer = 5
io:format("Tallet mitt er ~b og det dobbelte er ~b.", [nummer, nummer*2])
```

Dette vil gi følgende utdata:

```
Tallet mitt er 5 og det dobbelte er 10.
```

Du kan også bruke "io_lib:format" for å formatere utdata som en streng, og på den måten få mer kontroll over hvordan utdataen vises. Se dokumentasjonen for mer informasjon om hvordan du kan bruke disse funksjonene for å få ønsket utdata.

## Se også

- [Gleam dokumentasjon: Debugging](https://gleam.run/book/tutorials-and-guides/debugging.html)
- [Gleam dokumentasjon: IO modulen](https://gleam.run/modules/io.html)
- [Learn Gleam: Printing Debug Output](https://medium.com/learn-gleam/debugging-101-printing-debug-output-ea5c7f3b7cf9) (på engelsk)