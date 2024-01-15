---
title:                "Utskrift av feilrettingsresultater"
html_title:           "Clojure: Utskrift av feilrettingsresultater"
simple_title:         "Utskrift av feilrettingsresultater"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen engasjere seg i å skrive ut feilsøkingsutgang? Det er en vanlig praksis for å få mer innsikt i koden og identifisere feil og problemer som kan oppstå under kjøring.

## Hvordan

For å skrive ut feilsøkingsutgang i Clojure, kan du bruke funksjonen `prn`. Dette vil skrive ut hvilken som helst form for data som en lesbar streng i terminalen.

```
Clojure
(prn "Dette er en tekststreng")
; Dette er en tekststreng
```

Du kan også skrive ut datastrukturer, som for eksempel vektorer og kart, ved hjelp av `prn` funksjonen.

```
Clojure
(prn [1 2 3 4])
; [1 2 3 4]

(prn {:navn "Mia" :alder 25})
; {:navn "Mia", :alder 25}
```

Hvis du vil skrive ut mer spesifikk informasjon, kan du bruke `println` funksjonen og inkludere variabler eller ekstra tekst for å gi mer kontekst.

```
Clojure
(def navn "Maria")
(def alder 30)

(println "Brukeren heter" navn "og er" alder "år gammel.")
; Brukeren heter Maria og er 30 år gammel.
```

## Dypdykk

En annen nyttig metode for å skrive ut feilsøkingsutgang er ved hjelp av `clojure.pprint` biblioteket. Dette gir mer detaljert og organiseret utdata, spesielt for komplekse datastrukturer.

For eksempel, hvis vi har et kart med flere lag, kan vi bruke `pprint` funksjonen for å skrive ut det organiserte og leselige utdataet.

```
Clojure
(require '[clojure.pprint :refer [pprint]])

(def bruker {:navn "Sara"
             :adresse {:gate "Hovedveien 10"
                       :postnummer 12345
                       :by "Oslo"}})

(pprint bruker)
; {:navn "Sara",
;  :adresse {:gate "Hovedveien 10",
;            :postnummer 12345,
;            :by "Oslo"}}
```

Ved å inkludere `:linear` argumentet i `pprint` funksjonen, kan vi få utskrift av datastrukturen på en enklere og mer kompakt måte.

```
Clojure
(pprint bruker :linear)
; {:navn "Sara",
;  :adresse {:gate "Hovedveien 10",
;            :postnummer 12345,
;            :by "Oslo"}}
```

## Se også

- [Clojure Official Documentation](https://clojure.org/)
- [Effective Debugging Techniques in Clojure](https://blog.codeship.com/effective-debugging-techniques-clojure/)
- [Mastering Clojure's Debugging Techniques](https://practicalli.github.io/clojure/development/debugging/)