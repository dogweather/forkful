---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:57.099731-07:00
description: "Hoe te: Clojure is een beetje een asceet: geen ingebouwde stringinterpolatie.\
  \ Echter, `str` en `format` zijn de go-to voor dynamische strings.\u2026"
lastmod: '2024-04-05T21:53:50.430494-06:00'
model: gpt-4-0125-preview
summary: Clojure is een beetje een asceet.
title: Een string interpoleren
weight: 8
---

## Hoe te:
```Clojure
;; Basis met `str` en `format`
(def name "World")
(str "Hallo, " name "!")  ; => "Hallo, World!"

;; Gebruikmakend van `format`, vergelijkbaar met printf-stijl formatting
(format "Vaarwel, %s!" name)  ; => "Vaarwel, World!"

;; Clojure heeft geen ingebouwde stringinterpolatie zoals andere talen,
;; maar we kunnen creatief zijn met `str` en `format`.
```

## Diepe Duik:
Clojure is een beetje een asceet: geen ingebouwde stringinterpolatie. Echter, `str` en `format` zijn de go-to voor dynamische strings. Ontstaansgeschiedenis? Het simplisme ethos van Clojure. Het vertrouwt erop dat we zelf wel uit de voeten kunnen met stringconstructie.

Voor alternatieven, betreed de wereld van templating: `clostache` (een Clojure-implementatie van Mustache) of `hiccup` voor HTML-contexten. Ze komen van pas wanneer `str` en `format` te primitief aanvoelen.

Onder de motorkap delegeren `format` aan Java's `String.format`, een feit dat Clojure’s superkracht in Java-interoperabiliteit illustreert. Dus, hoewel je de zoetigheid niet krijgt, heb je de spierkracht van Java wanneer je het nodig hebt.

## Zie Ook:
- Clojure Docs over `str`: https://clojuredocs.org/clojure.core/str
- Clojure Docs over `format`: https://clojuredocs.org/clojure.core/format
- clostache GitHub-repo: https://github.com/fhd/clostache
- hiccup GitHub-repo: https://github.com/weavejester/hiccup
