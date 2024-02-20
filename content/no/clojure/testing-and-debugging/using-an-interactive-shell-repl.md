---
date: 2024-01-26 04:13:18.298276-07:00
description: "REPL, eller Read-Eval-Print Loop, er et programmeringsmilj\xF8 for dynamisk\
  \ testing av Clojure-kode stykke for stykke. Kodeutviklere bruker det for\u2026"
lastmod: 2024-02-19 22:04:59.688060
model: gpt-4-0125-preview
summary: "REPL, eller Read-Eval-Print Loop, er et programmeringsmilj\xF8 for dynamisk\
  \ testing av Clojure-kode stykke for stykke. Kodeutviklere bruker det for\u2026"
title: Bruke et interaktivt skall (REPL)
---

{{< edit_this_page >}}

## Hva & Hvorfor?
REPL, eller Read-Eval-Print Loop, er et programmeringsmiljø for dynamisk testing av Clojure-kode stykke for stykke. Kodeutviklere bruker det for øyeblikkelig tilbakemelding, iterativ utvikling og rask eksperimentering uten overheaden av å kompilere eller sette opp et komplett prosjektmiljø.

## Hvordan:
Start med å lansere REPL:

```Clojure
user=> (println "Hello, REPL!")
Hello, REPL!
nil
```

Definer en funksjon og prøv den ut:
```Clojure
user=> (defn greet [name] (str "Hello, " name "!"))
#'user/greet
user=> (greet "Clojure Programmer")
"Hello, Clojure Programmer!"
```

Eksperimenter med datastrukturer:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## Dypdykk
REPL er nøkkelen til Lisp-familiens interaktive utviklingsfilosofi, og Clojure, en moderne Lisp-dialekt, gjør stor bruk av dette verktøyet. Det går tilbake til den første Lisp REPL på slutten av 1950-tallet. Alternativer i andre språk inkluderer Pythons tolker og Node.js's konsoll, men Clojures REPL har første klasse status og er integrert i arbeidsflyten.

En Clojure REPL-økt kan integreres i ulike miljøer som kommandolinje, IDEer (som IntelliJ med Cursive, eller Emacs med CIDER) eller nettleserbaserte verktøy som Nightcode. I en dypere forstand, gir REPL utvikleren muligheten til å manipulere språkets konstruksjoner i kjøretiden og bære tilstander gjennom ulike transformasjoner, ofte fører til utforskende programmering og mer robust kode.

REPLs funksjonalitet skinner med verktøy som `lein repl` eller `clj`, som tillater for avhengighetsstyring, ulike tillegg, og prosjektspesifikke tilpasninger, som fører til en mer produktiv og fleksibel utviklingsprosess.

## Se også
- Den offisielle Clojure-websiden guide om REPL: https://clojure.org/guides/repl/introduction
- Rich Hickeys snakk om REPL-drevet utvikling: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Praktisk Clojure: bruk av REPL for iterativ utvikling: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
