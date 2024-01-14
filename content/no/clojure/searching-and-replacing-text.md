---
title:    "Clojure: Søking og erstatning av tekst"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger må vi gjøre endringer i tekstfiler, enten for å rette feil eller å endre visse deler av teksten. I Clojure er det enkelt å søke og erstatte tekst ved hjelp av noen få linjer med kode. Dette sparer oss for manuelt arbeid og gjør det enkelt å gjøre endringer i store tekstfiler.

## Slik gjør du det
For å søke og erstatte tekst i Clojure må vi bruke funksjonen ```clojure.string/replace```. Denne funksjonen tar tre argumenter, den første er teksten vi vil gjøre endringer i, den andre er det vi vil erstatte, og den siste er hva vi vil erstatte det med. La oss se på noen eksempler:

```
;; Erstatt alle forekomster av ordet "hund" med ordet "katt"
(clojure.string/replace "Jeg har en hund og en katt" "hund" "katt")
;; Output: "Jeg har en katt og en katt"

;; Erstatt første forekomst av "1" med "9"
(clojure.string/replace "1,2,3,4,1" "1" "9" 1)
;; Output: "9,2,3,4,1"
```

Vi kan også bruke regulære uttrykk for å søke og erstatte tekst. For dette må vi bruke funksjonen ```clojure.string/replace-first```, som tar inn de samme argumentene som ```replace```, men også et fjerde argument som er et regulært uttrykk. Her er et eksempel på å erstatte et tall med "X" i en tekststreng:

```
;; Erstatt første forekomst av et tall med "X"
(clojure.string/ replace-first "abc123def456" #"\d" "X")
;; Output: "abcX23def456"
```

## Dykk dypere
Som nevnt i forrige avsnitt, kan vi bruke regulære uttrykk når vi søker og erstatter i Clojure. Dette gir oss muligheten til å være mer presise i hva vi søker etter, og hva vi ønsker å erstatte det med. For å lære mer om hvordan man bruker regulære uttrykk i Clojure, kan du sjekke ut [Clojure regex](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/core/reducers.clj).

Vi kan også bruke funksjonen ```replace```, som vi brukte i eksemplene over, på flere datatyper enn bare tekststrenger. Dette inkluderer for eksempel lister og maps. Hvis vi ønsker å gjøre endringer i et map, må vi bruke ```clojure.walk/postwalk``` funksjonen. Dette lar oss gå gjennom og endre alle verdier i et map. Mer informasjon om dette kan du finne [her](https://clojure.org/reference/data_structures#Maps).

## Se også
- [Clojure regex](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/core/reducers.clj)
- [Clojure datastrukturer](https://clojure.org/reference/data_structures)