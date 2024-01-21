---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:50:38.780409-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av en streng lar deg putte variabler eller uttrykk direkte inn i en streng. Programmerere gjør dette for å dynamisk bygge strenger, som gjør koden mer leselig og hendig.

## Hvordan gjøre det:
```Clojure
;; Bruke format for enkel strenginterpolering
(def navn "Ola")
(println (format "Hei, mitt navn er %s!" navn))
;; Output: Hei, mitt navn er Ola!

;; Bruke str for å sammenslå strenger og variabler
(defn hilsen [navn]
  (str "Hei " navn ", hvordan har du det?"))
(println (hilsen "Kari"))
;; Output: Hei Kari, hvordan har du det?
```

## Dypdykk
I tidligere programmeringsspråk som C, ble strenginterpolering ofte gjort med printf-familien av funksjoner. Clojure, skjønt, er et moderne Lisp og tilbyr ikke innebygd strenginterpolering på samme måte. Men det gir `format` og `str` for tilsvarende funksjonalitet. Mens `format` bruker Java's `String.format()` og tar formateringsstrenger, kombinerer `str` rett og slett verdier sammen til en enkelt streng.

Et alternativ er å bruke en template-bibliotek som Selmer for mer avansert formatering som kan inkludere logikk. For å implementere noe mer lik traditionell interpolering, kan makroer brukes til å utvide språkets syntaks.

Clojure har et sterk fokus på enkelhet og funksjonalitet. Strenginterpolering, slik noen andre språk tilbyr det, kan betraktes som unødvendig magi i denne sammenhengen.

## Se også
- [Clojure str](https://clojuredocs.org/clojure.core/str)
- [Clojure format](https://clojuredocs.org/clojure.core/format)
- [Selmer template engine](https://github.com/yogthos/Selmer)
- [Strenginterpolering med makroer i Clojure](https://stackoverflow.com/questions/20139943/how-do-i-do-string-interpolation-in-clojure)