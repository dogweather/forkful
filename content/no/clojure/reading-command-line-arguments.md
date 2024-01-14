---
title:    "Clojure: Lesing av kommandolinjeargumenter"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese argumenter fra kommandolinjen kan være en veldig viktig ferdighet for en Clojure-programmerer. Dette kan hjelpe deg med å lage mer dynamiske og fleksible programmer som kan tilpasses ved kjøretid. Så, hvorfor ikke lære å gjøre det?

## Slik gjør du det

Det er enkelt å lese argumenter fra kommandolinjen i Clojure. Alt du trenger å gjøre er å bruke funksjonen ```clojure.core/command-line-args``` og angi filen med argumenter som et argument. La oss se på et eksempel:

```clojure
(ns command-line
  (:require [clojure.core :refer [command-line-args]]))
  
(def args (command-line-args))
  
(println "Navnet på programmet er: " (nth args 0))
(println "Første argumentet er: " (nth args 1))
```

I dette eksempelet bruker vi funksjonen ```command-line-args``` for å lese argumentene som ble gitt ved kjøretid. Disse argumentene er lagret i variabelen ```args``` som vi deretter kan bruke til å få tilgang til hvert enkelt argument. Så hvis vi kjører dette programmet med følgende kommandolinje:

```bash
lein run command-line.clj Clojure Programmerer
```

vil vi få følgende utskrift:

```
Navnet på programmet er: command-line.clj
Første argumentet er: Clojure Programmerer
```

Som du kan se, har vi fått tilgang til både navnet på programmet og det første argumentet som ble gitt i kommandolinjen.

## Dypdykk

Det er også mulig å lese inn argumenter som flagg (flags) i form av ```--navn-verdi``` eller ```-navn verdi```. Dette kan være nyttig for å gi ulike innstillinger til programmet ditt. La oss se på et eksempel på hvordan vi kan gjøre dette:

```clojure
(ns command-line
  (:require [clojure.core :refer [command-line-args]]))
  
(def args (command-line-args))
  
(println "Navnet på programmet er: " (nth args 0))

(if (= "-verbose" (nth args 1))
  (println "Programmet kjøres i verbose modus."))

```

I dette eksempelet sjekker vi om det andre argumentet er ```-verbose``` og hvis det er tilfelle, vil programmet gi tilbakemelding om at det kjører i verbose modus. Slik kan vi enkelt legge til ulike funksjoner eller innstillinger basert på argumentene som blir gitt ved kjøretid.

## Se også

- [Clojure.org - Command Line Args](https://clojure.org/reference/command_line_args)
- [ClojureDocs - command-line-args](https://clojuredocs.org/clojure.core/command-line-args)
- [Repl.it - Online Clojure Compiler](https://repl.it/languages/clojure)