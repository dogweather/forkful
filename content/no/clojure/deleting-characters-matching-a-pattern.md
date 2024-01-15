---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "Clojure: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Hvorfor
Noen ganger ønsker man å fjerne spesifikke tegn fra en streng, for eksempel for å formatere eller rense data. Ved å lære hvordan man sletter tegn som matcher et visst mønster i Clojure, kan man effektivt håndtere slike situasjoner.

##Slik Gjør Du
```Clojure
(def my-string "Hei, jeg heter Anna!")

;;Slette alle mellomrom fra strengen
(clojure.string/replace my-string #"\s" "")

;;Output: "Hei,jeg haterAnna!"

;;Slette alle tall fra strengen
(clojure.string/replace my-string #"\d" "")

;;Output: "Hei, jeg heter Anna!"
```
Eksemplene bruker funksjonen `clojure.string/replace` og et regulært uttrykk som spesifiserer hvilke tegn som skal slettes fra strengen. I det første eksempelet sletter vi alle mellomrom (representert med `\s`), og i det andre slettes alle tall (representert med `\d`). Ved å erstatte det siste argumentet med ønsket mønster, kan man tilpasse funksjonen til sine behov.

##Dypere Dykk
Når man bruker regulære uttrykk for å slette tegn, må man være klar over at uttrykket matcher alle forekomster av det spesifikke mønsteret i strengen. Det kan være nyttig å bruke anker-tegn (`^` og `$`) for å begrense søket til å kun gjelde på slutten eller begynnelsen av strengen. I tillegg kan man bruke gruppering (`()`) for å beholde deler av teksten som matcher mønsteret.

##Se Også
- [Clojure Dokumentasjon om `clojure.string`](https://clojuredocs.org/clojure.string)
- [Regulære Uttrykk i Clojure](https://clojure.org/guides/learn/regular_expressions)