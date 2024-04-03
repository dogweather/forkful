---
date: 2024-01-20 17:50:26.725662-07:00
description: "Interpolering av str\xE4ng \xE4r processen d\xE4r man s\xE4tter in uttryck\
  \ eller variabler in i str\xE4ngar. Programmerare g\xF6r det f\xF6r att dynamiskt\
  \ bygga str\xE4ngar \u2013\u2026"
lastmod: '2024-03-13T22:44:37.511569-06:00'
model: gpt-4-1106-preview
summary: "Interpolering av str\xE4ng \xE4r processen d\xE4r man s\xE4tter in uttryck\
  \ eller variabler in i str\xE4ngar."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Vad & Varför?
Interpolering av sträng är processen där man sätter in uttryck eller variabler in i strängar. Programmerare gör det för att dynamiskt bygga strängar – oftast för att visa information till användaren eller för att skapa dynamisk kod.

## Så här gör du:
```Clojure
;; Använda format
(def name "världen")
(println (format "Hej, %s!" name))

;; Output: Hej, världen!
```

```Clojure
;; Använda str
(def age 30)
(println (str "Jag är " age " år gammal."))

;; Output: Jag är 30 år gammal.
```
## Fördjupning
Interpolering av strängar är inte en inbyggd funktion i Clojure på samma sätt som i vissa andra språk. Historiskt, i Lisp-dialekter var man tvungen att använda funktioner som `format` eller konkatenering med `str` för att uppnå samma resultat. Clojure håller sig till denna tradition. Trots att det inte finns inbyggd interpolering, kan bibliotek som `clojure.string` eller tillägg som `strfmt` ge ett liknande beteende med mer smidighet. Implementationsdetaljer centreras kring JVM, eftersom Clojure är en dialekt av Lisp som är utformad för att köras på Java Virtual Machine.

## Se Mer
- Officiell Clojure documentation för `str`: https://clojuredocs.org/clojure.core/str
- Officiell Clojure documentation för `format`: https://clojuredocs.org/clojure.core/format
- GitHub-repo för `strfmt` biblioteket: https://github.com/dbriskin/strfmt
