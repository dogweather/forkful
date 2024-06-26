---
date: 2024-01-20 17:50:38.780409-07:00
description: "Hvordan gj\xF8re det: I tidligere programmeringsspr\xE5k som C, ble\
  \ strenginterpolering ofte gjort med printf-familien av funksjoner. Clojure, skj\xF8\
  nt, er et\u2026"
lastmod: '2024-04-05T22:50:54.397315-06:00'
model: gpt-4-1106-preview
summary: "I tidligere programmeringsspr\xE5k som C, ble strenginterpolering ofte gjort\
  \ med printf-familien av funksjoner."
title: Interpolering av en streng
weight: 8
---

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
