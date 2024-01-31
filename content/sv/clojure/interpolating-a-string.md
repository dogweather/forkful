---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:50:26.725662-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

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
