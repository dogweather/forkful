---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Clojure: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver er en vanlig oppgave for programmerere. Dette innebærer å endre alle bokstavene i en streng til små bokstaver. Dette kan være nyttig for å sikre at brukerens inntasting er i riktig format eller for å sammenligne to strenger uavhengig av store og små bokstaver.

## Hvordan:
Convert en streng til små bokstaver er enkelt i Clojure. Du kan bruke funksjonen `lower-case`eller metoden `.toLowerCase()` på en streng. Se eksemplene nedenfor for å se hvordan dette gjøres:

```Clojure
; Ved hjelp av lower-case-funksjonen
(lower-case "HELLO") ; Output: "hello"

; Ved hjelp av .toLowerCase() -metoden
(.toLowerCase "HELLO") ; Output: "hello"
```

## Dypdykk:
Konvertering av en streng til små bokstaver har vært en viktig del av programmering i mange år. Før moderne programmeringsspråk som Clojure, ble dette gjort ved å bruke ulike metoder for å endre bokstaver til små bokstaver, som å trekke fra 32 fra ASCII-verdien eller utføre XOR-operasjoner.

Alternativene til å bruke `lower-case`-funksjonen i Clojure inkluderer også å bruke en løkke for å gå gjennom hver enkelt bokstav i en streng og endre til små bokstaver ved hjelp av `char-utils` biblioteket.

`lower-case`-funksjonen er implementert ved hjelp av Java-metoden `toLowerCase()` som er en del av `String`-klassen. Dette betyr at du også kan bruke denne metoden på Java-strenger i Clojure.

## Se også:
- Clojure.org: [lower-case](https://clojuredocs.org/clojure.core/lower-case)
- Java API: [String.toLowerCase()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())