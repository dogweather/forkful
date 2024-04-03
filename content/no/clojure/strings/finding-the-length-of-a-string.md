---
date: 2024-01-20 17:47:28.998504-07:00
description: "Hvordan gj\xF8re det: For \xE5 finne lengden av en streng i Clojure,\
  \ bruk `count` eller `length` funksjonen. `count` er mer idiomatic for Clojure."
lastmod: '2024-03-13T22:44:40.393707-06:00'
model: gpt-4-1106-preview
summary: "For \xE5 finne lengden av en streng i Clojure, bruk `count` eller `length`\
  \ funksjonen."
title: "Finn lengden p\xE5 en streng"
weight: 7
---

## Hvordan gjøre det:
For å finne lengden av en streng i Clojure, bruk `count` eller `length` funksjonen. `count` er mer idiomatic for Clojure.

```Clojure
;; Bruk `count` for å finne lengden av en streng
(count "Hei, Norge!")
;; => 11

;; `length` funksjonen eksisterer ikke i standard Clojure, men kan bli definert
(defn length [s] (.length s))
(length "Hei, Norge!")
;; => 11

;; Med `count` på en tom streng
(count "")
;; => 0
```

## Dypdykk
I de tidlige dagene av programmering, var det å håndtere strenger ofte krevende grunnet minnebegrensninger. I moderne programmeringsspråk, inkludert Clojure, er funksjonen for å finne lengden av en streng innebygget og optimalisert.

Mens `count` er mer idiomatic for Clojure og fungerer på alle sekvensielle typer, har noen andre språk spesifikke funksjoner eller egenskaper, slik som `.length` i Java, noe som er årsaken til at du kan definere en lignende funksjon i Clojure ved å tilkalle Java-metoden direkte, som vist over.

Hvis ytelse blir kritisk, husk at `count` i Clojure har en konstant tidskompleksitet for strenger, men vil variere for andre samlinger som for eksempel lister, hvor operasjonen kan bli lineær.

## Se Også
- Clojure's offisielle dokumentasjon for `count`: [https://clojuredocs.org/clojure.core/count](https://clojuredocs.org/clojure.core/count)
- En guide til Clojure strenger og deres funksjoner: [https://clojure.org/guides/weird_characters#_working_with_strings](https://clojure.org/guides/weird_characters#_working_with_strings)
