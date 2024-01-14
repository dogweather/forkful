---
title:                "Clojure: Ekstrahering av delstrenger"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med tekstbehandling i Clojure, kan du ofte ha behov for å hente ut deler av en streng (substring). Dette kan være nyttig for å manipulere data eller for å få en bedre forståelse av teksten. I denne bloggposten vil vi dykke dypere ned i hvordan vi kan ekstrahere substrings i Clojure, og hvorfor det kan være nyttig.

## Hvordan

Det første vi må gjøre er å importere funksjonen "subs" fra "clojure.string" biblioteket. Deretter kan vi bruke "subs" til å hente ut en delstreng basert på en plassering og lengde:

```Clojure
(require '[clojure.string :as str])

(def tekst "Denne teksten er ment for å demonstrere substring funksjonen.")

(str/subs tekst 6 13) ; dette vil returnere "teksten"
```

Vi kan også bruke "subs" til å hente ut en delstreng basert på et bestemt mønster. For eksempel, hvis vi ønsker å hente ut alle ord som starter med bokstaven "d" fra en tekst, kan vi gjøre følgende:

```Clojure
(def tekst "Ord som starter med bokstaven d er for eksempel dette og dette.")

(str/subs tekst #"(d\w+)") ; dette vil returnere "dette og dette"
```

## Dypdykk

Nå når vi har sett på noen eksempler på hvordan vi kan bruke "subs" funksjonen, la oss dykke dypere inn i hva som skjer under overflaten. Den første parameteren som vi gir til "subs" er strengen vi ønsker å hente ut delstrenger fra. Den andre parameteren er plasseringen hvor vi ønsker å starte (den første bokstaven vil være 0). Den tredje parameteren er lengden på delstrengen vi ønsker å hente ut.

Hvis vi bruker et regex-mønster som andre parameter, vil "subs" returnere alle matchende delstrenger. Merk at "subs" funksjonen er null-basert, som betyr at første match vil være på indeks 0.

## Se Også

- [Clojure Documentation - String Functions](https://clojuredocs.org/clojure.string/subs)
- [Clojure Cookbook - Extracting Substrings](https://clojure-cookbook.github.io/string/extracting-substrings.html)
- [Regular Expressions in Clojure](https://luminusweb.com/docs/clojure/regular-expressions.html)