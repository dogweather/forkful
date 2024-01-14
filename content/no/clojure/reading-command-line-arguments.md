---
title:                "Clojure: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er en erfaren Clojure-programmerer eller bare begynner å lære språket, er det viktig å forstå hvordan du kan lese kommandolinjeargumenter i dine Clojure-programmer. Dette er en viktig ferdighet som kan hjelpe deg med å lage mer fleksible og tilpasningsdyktige programmer. Plus, det kan hjelpe til med debugging og testing av kode raskere.

## Hvordan

For å lese kommandolinjeargumenter i Clojure, kan du bruke funksjonen `command-line-args`. Denne funksjonen tar ikke imot noen argumenter og returnerer en liste med alle argumentene gitt ved kjøring av programmet.

```Clojure
(def args (command-line-args))
(println "Første argument:" (first args))
(println "Andre argument:" (second args))

;; Eksempel på kjøring av programmet med argumenter:
;; $ lese-argumenter.clj argument1 argument 2
;; Første argument: argument1
;; Andre argument: argument2
```

Som du kan se, blir argumentene lagret i en liste og du kan hente dem ut ved å referere til indeksposisjonen. Det er viktig å merke seg at argumentene alltid vil være strenger, uavhengig av hva slags data de representerte ved innlesing.

## Dypdykk

Det er viktig å være oppmerksom på at rekkefølgen på argumentene som blir gitt til `command-line-args` vil være den samme som de ble skrevet inn ved kjøring av programmet. I tillegg, hvis du ønsker å lese inn flere argumenter som en liste eller kart, kan du bruke funksjonen `clojure.java.io/parse-opts`. Denne funksjonen tar imot argumentene og en spesifisert liste av mulige argumenttyper og returnerer et kart med tilhørende verdier.

```Clojure
(def args (command-line-args))
(def opts (parse-opts args [["-f" "--file" "File name"] ["-v" "--verbose" "Verbose output"]]))

(println "Filnavn: " (:file opts))
(println "Verbose?: " (:verbose opts))

;; Eksempel på kjøring av programmet med argumenter:
;; $ lese-argumenter.clj -f hello.txt --verbose
;; Filnavn: hello.txt
;; Verbose?: true
```

Som du kan se i eksemplet ovenfor, kan du til og med spesifisere hvilken type data du forventer at argumentet vil være. Dette kan hjelpe til med å håndtere uønskede feil som kan oppstå fra ugyldige argumenter.

## Se Også

- Clojure Cheatsheet: https://clojure.org/api/cheatsheet