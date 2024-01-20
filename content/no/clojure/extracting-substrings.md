---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Utdrag av delstrenger er prosessen med å hente en liten del av en streng. Programmerere gjør dette for å behandle eller manipulere spesifikke data innen en større datasett.

## Hvordan:

Hvis vi for eksempel ønsker å hente en del av en streng i Clojure, bruker vi `subs` funksjonen. Her er et eksempel:

```Clojure
(defn delstreng [streng start slutt]
  (subs streng start slutt))

(defn hoved []
  (println (delstreng "Hei der, verden!" 0 7)))

(hoved)
```
Når du kjører denne koden, ville utmatningen være:

```Clojure
"Hei de"
```
## Dypdykking

Historisk sett har funksjonen for å trekke ut delstrenger vært en del av mange programmeringsspråk, inkludert Clojure. Alternativt kan ‘slice’ metoden benyttes i noen språk, men i Clojure er `subs` den primære metoden. 

Ved implementering skal du være klar over at indeksposisjonen i strenger starter ved 0, ikke 1. Det vil si hvis du ønsker å starte fra den første bokstaven i strengen, ville startindeksen være 0.

## Se Også

For mer informasjon om håndtering av strenger i Clojure, sjekk ut følgende kilder:

1. [Offisiell Clojure dokumentasjon](https://clojure.org/)
2. [Clojure for nybegynnere](https://www.braveclojure.com/)
3. [Clojure stregn funksjoner](http://clojuredocs.org/quickref/Clojure%20Core#Strings)