---
title:                "Clojure: Lese kommandolinje argumenter"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å kunne lese kommandolinjeargumenter er en viktig ferdighet for enhver Clojure-programmerer. Det lar deg lage mer fleksible og dynamiske programmer som kan ta imot input fra brukeren i form av argumenter.

## Slik gjør du det

Det er enkelt å lese kommandolinjeargumenter i Clojure. Først må du importere biblioteket `clojure.java.shell`, som lar deg kalle på systemkommandoer og lese resultatet som en streng.

```
(ns clojure-blog.core
  (:require [clojure.java.shell :refer [sh]]))
```

Deretter kan du bruke funksjonen `sh` for å kalle på systemkommandoen `args` som gir deg en liste med alle de gitte argumentene. For eksempel, om du kjørte programmet ditt med argumentet "hello", så ville `sh` returnere følgende liste:

`````Clojure
(sh "args")
;; => {:exit 0 :out "hello" :err ""}
`````

Om du ønsker å lese flere argumenter, så kan du legge dem til etter systemkommandoen `args`. For eksempel, om du vil lese det første og tredje argumentet, så kan du gjøre som følger:

`````Clojure
(sh "args" "first" "third")
;; => {:exit 0 :out "first third" :err ""}
`````

## Dypdykk

Det finnes også andre måter å lese kommandolinjeargumenter på i Clojure, som for eksempel ved å bruke funksjonen `clojure.main` og argumentet `-main`. Dette kan være nyttig om du ønsker å kjøre programmet ditt som et kommandolinjeprogram. Du kan lese mer om dette her: https://clojure.org/guides/deployment

## Se også

- https://clojure-toolbox.com/
- https://clojure.org/
- https://www.honeysql.org/