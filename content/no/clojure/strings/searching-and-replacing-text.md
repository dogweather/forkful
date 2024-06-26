---
date: 2024-01-20 17:58:03.253255-07:00
description: "Hvordan: Historisk sett oppsto behovet for s\xF8k og erstatning tidlig\
  \ i databehandling for \xE5 h\xE5ndtere tekstfiler effektivt. I Lisp-universet,\
  \ som Clojure er\u2026"
lastmod: '2024-04-05T21:53:41.366389-06:00'
model: gpt-4-1106-preview
summary: "Historisk sett oppsto behovet for s\xF8k og erstatning tidlig i databehandling\
  \ for \xE5 h\xE5ndtere tekstfiler effektivt."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

## Hvordan:
```Clojure
; Finn og erstatt enkeltstående streng
(clojure.string/replace "Hei verden!" "verden" "Norge")
; => "Hei Norge!"

; Bruk regulære uttrykk for å finne og erstatte mønstre
(clojure.string/replace "Epler, bananer, kirsebær" #"[aeiou]" "-")
; => "Epl-r, b-n-n-r, k-rs-b-r"

; Gjør komplekse erstatninger med en funksjon
(clojure.string/replace "7 hunder, 3 katter" #"\d" #(str (inc (Integer/parseInt %))))
; => "8 hunder, 4 katter"
```

## Dypdykk
Historisk sett oppsto behovet for søk og erstatning tidlig i databehandling for å håndtere tekstfiler effektivt. I Lisp-universet, som Clojure er en del av, har string-manipulasjon vært en integrert del siden 1950-tallet. Clojure forenkler prosessen med innebygde funksjoner som `clojure.string/replace`.

Alternativer inkluderer å bruke `sed` i Unix-baserte systemer, eller i et mer moderne utviklingsmiljø, innebygde metoder i teksteditorer som Emacs eller Vim.

Implementeringsdetaljer avhenger ofte av brukstilfellet. For eksempel, regulære uttrykk (regex) tillater avanserte mønstre og erstatninger, men kan være tregere med store tekstmengder. Det er også viktig å være obs på "greedy" vs "non-greedy" matching i regex, som påvirker hvilke deler av teksten som blir erstattet.

## Se også
- Clojure's string API: [https://clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string)
- Online regex tester: [https://regexr.com/](https://regexr.com/)
- Emacs manual on Search and Replace: [https://www.gnu.org/software/emacs/manual/html_node/emacs/Search.html](https://www.gnu.org/software/emacs/manual/html_node/emacs/Search.html)
