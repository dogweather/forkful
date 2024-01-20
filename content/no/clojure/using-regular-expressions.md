---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk finner tekst etter mønstre. Programmerere bruker det for å søke, erstatte og validere data raskt og fleksibelt.

## Hvordan:
```Clojure
;; Søker etter ordet "Clojure"
(re-find #"\bClojure\b" "Lær mer om Clojure programmering!")

;; Skriver ut: "Clojure"
```

```Clojure
;; Splitter string på komma
(re-seq #"[^,]+" "clojure,java,python,ruby")

;; Skriver ut: ("clojure" "java" "python" "ruby")
```

```Clojure
;; Sjekker om en e-post er gyldig
(boolean (re-matches #"[^\s@]+@[^\s@]+\.[^\s@]+" "din.email@domene.no"))

;; Skriver ut: true
```

## Dypdykk
De første regulære uttrykkene ble brukt på 1950-tallet. Alternativer til regulære uttrykk inkluderer streng-manipulasjons-funksjoner og spesialiserte biblioteker, men de har ofte mindre makt. Clojure bruker Javas Pattern-klassen, så ytelsen er lik Java sin.

## Se Også
- ClojureDocs (https://clojuredocs.org/)
- Java Pattern dokumentasjon (https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- "Mastering Clojure" av Akhil Wali (for videre lesing om Clojure)