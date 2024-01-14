---
title:    "Clojure: Sammenligning av to datoer"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

Du har sikkert opplevd situasjoner der du trenger å sammenligne to datoer, enten det er for å sortere data eller filtrere ut informasjon. Men hvordan gjør man det i Clojure? I denne bloggposten vil vi vise deg akkurat det.

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave i enhver programmeringsverden. Det kan være nyttig å vite om en dato kommer før eller etter en annen, eller om de er like. Dette kan hjelpe deg å organisere data eller avgjøre relevansen av informasjon.

## Hvordan gjøre det

I Clojure er det litt annerledes å sammenligne datoer enn i andre språk, da datoer er representert som tall i millisekunder siden 1. januar 1970. Men bekymre deg ikke, det er fortsatt en enkel prosess.

La oss si at vi har to datoer som vi ønsker å sammenligne:

```Clojure
(def date1 (java.util.Date. 2020 07 15))
(def date2 (java.util.Date. 2020 07 20))
```

For å sammenligne disse, bruker vi funksjonen `compare` og gir den de to datoene som argumenter. Denne funksjonen returnerer enten -1, 0 eller 1, avhengig av om `date1` kommer før, er lik eller kommer etter `date2`. La oss se på et eksempel:

```Clojure
(def result (compare date1 date2))
```

Hvis vi nå printer ut `result`, vil vi få utskriften `-1`, siden `date1` kommer før `date2` i kronologisk rekkefølge.
    
Dette fungerer også med klokkeslett. Hvis du ønsker å sammenligne datoer med klokkeslett, må du først konvertere disse til tidsstempel i millisekunder ved å bruke funksjonen `(.getTime date1)`.

## Dypdykk

For å forstå hvordan `compare` fungerer, er det nyttig å vite at det implementerer `Comparable` interfacet. Dette betyr at det sammenligner objekter basert på en naturlig ordning. For å forstå hvordan dette fungerer, kan du ta en titt på [offisiell dokumentasjon](https://clojure.org/reference/java_interop#_implementing_interfaces).

## Se også

Her er noen nyttige ressurser for å lære mer om å sammenligne datoer i Clojure:

- [ClojureDocs: compare](https://clojuredocs.org/clojure.core/compare)
- [Official Clojure Documentation: Java Interop](https://clojure.org/reference/java_interop)
- [Example of comparing dates in Clojure](https://gist.github.com/mecampbellsoup/c6555c4514b614f433672fd9392e0978)