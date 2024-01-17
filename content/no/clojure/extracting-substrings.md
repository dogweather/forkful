---
title:                "Uttrekking av delstrenger"
html_title:           "Clojure: Uttrekking av delstrenger"
simple_title:         "Uttrekking av delstrenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Utvinning av substringer er prosessen med å isolere en del av en tekststreng basert på et spesifikt sett med kriterier. Programmere bruker denne teknikken for å manipulere data og finne spesifikke deler av en tekststreng som er relevant for deres kode.

## Hvordan:
```Clojure
;; Eksempel på utvinning av substring fra en tekststreng
(def tekst "Dette er en tekststreng.")

;; Utvinning av tekst fra indeks 11 til slutten av strengen
(substring tekst 11)

;; Forventet output: "tekststreng."

;; Utvinning av tekst fra indeks 11 til 16
(substring tekst 11 16)

;; Forventet output: "tekst"

```

## Dykk dypere:
- Historisk kontekst: Utvinning av substringer har vært en viktig teknikk for å håndtere tekstbaserte data siden de tidligste programmingsdagene.
- Alternativer: Det finnes mange alternative måter å utvinne substringer på, som bruk av regex-uttrykk eller innebygde funksjoner i andre programmeringsspråk.
- Implementeringsdetaljer: I Clojure er substrings representert som vanlige tekststrenger, og kan derfor behandles som ethvert annet dataelement i språket.

## Se også:
- Dokumentasjon for Clojure's ```substring``` funksjon: https://clojuredocs.org/clojure.core/substring
- Alternativ metode for å utvinne substringer ved hjelp av regex-uttrykk: https://clojuredocs.org/clojure.string/replace-first