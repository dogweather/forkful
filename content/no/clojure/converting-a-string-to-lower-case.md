---
title:                "Clojure: Konvertere en streng til små bokstaver"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er et vanlig problem i programmering. Dette er nyttig i tilfeller der du trenger å sammenligne strenger uavhengig av store og små bokstaver, eller trenger å formatere en streng på en bestemt måte.

## Hvordan

For å konvertere en streng til små bokstaver i Clojure, kan du bruke funksjonen `lower-case` som tar en streng som argument og returnerer en ny streng med bare små bokstaver. Her er et eksempel:

```Clojure
(lower-case "HELLO WORLD")
```

Dette vil returnere strengen "hello world".

Hvis du har en liste med strenger, kan du bruke `map` -funksjonen for å påføre `lower-case` på hvert element i listen. Her er et eksempel:

```Clojure
(map lower-case ["HELLO" "WORLD"])
```

Dette vil returnere en liste med strenger ["hello" "world"].

## Dypdykk

Når du bruker `lower-case` på en streng som inneholder spesialtegn, vil disse ikke bli endret til små bokstaver. Dette er fordi Clojure følger Unicode-standard og håndterer spesialtegn på riktig måte. Hvis du trenger å konvertere spesialtegn til små bokstaver, kan du bruke funksjonen `lower-case*`, som også støtter en ekstra argument for å angi språk.

En annen ting å merke seg er at `lower-case` funksjonen er ikke-destruktiv, det vil si at den ikke endrer den originale strengen, men returnerer en ny streng med de konverterte bokstavene. Hvis du trenger å endre den originale strengen, kan du bruke `string/lower-case!` funksjonen fra standardbiblioteket `clojure.string`.

## Se Også

- [Clojure Dokumentasjon: lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [Clojure Dokumentasjon: lower-case*](https://clojuredocs.org/clojure.string/lower-case*)
- [Unicode-tabell](https://unicode-table.com/en/)