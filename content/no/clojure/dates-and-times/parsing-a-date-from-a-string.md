---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:54.026083-07:00
description: "\xC5 parse en dato fra en streng i Clojure handler om \xE5 konvertere\
  \ tekstlige representasjoner av datoer og tider til en mer brukbar form (f.eks.\
  \ Clojures\u2026"
lastmod: '2024-03-13T22:44:40.411347-06:00'
model: gpt-4-0125-preview
summary: "\xC5 parse en dato fra en streng i Clojure handler om \xE5 konvertere tekstlige\
  \ representasjoner av datoer og tider til en mer brukbar form (f.eks. Clojures\u2026"
title: Analysering av en dato fra en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse en dato fra en streng i Clojure handler om å konvertere tekstlige representasjoner av datoer og tider til en mer brukbar form (f.eks. Clojures DateTime-objekt). Denne prosessen er grunnleggende for databehandling, logging, eller enhver applikasjon som manipulerer tidspunktsdata, og tillater programmerere å utføre operasjoner, sammenligninger, eller manipulasjoner på datoer effektivt.

## Hvordan:
Clojure, som et JVM-språk, lar deg bruke Javas dato- og tidsbiblioteker direkte. La oss starte med den innebygde Java-interoperabiliteten og deretter utforske hvordan man utnytter et populært tredjepartsbibliotek, `clj-time`, for mer idiomatiske Clojure-løsninger.

### Bruke Java Interop
Clojure kan direkte benytte seg av Javas `java.time.LocalDate` for å parse datoer fra strenger:
```clojure
(require '[clojure.java.io :as io])

; Parse en dato ved hjelp av Java interop
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; Utdata: 2023-04-01
```

### Bruke clj-time
Et mer idiomatisk Clojure-bibliotek for å håndtere datoer og tider er `clj-time`. Det omslutter Joda-Time, et omfattende bibliotek for dato- og tidsoperasjoner. Først må du legge til `clj-time` i prosjektets avhengigheter. Slik parser du en datostreng med `clj-time`:

```clojure
; Husk å legge til [clj-time "0.15.2"] i project.clj under :dependencies

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; Definer en formatter
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; Utdata: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

Disse eksemplene demonstrerer grunnleggende datoparsing. Begge metodene er nyttige, men `clj-time` kan tilby en mer Clojure-sentrert tilnærming med tilleggsfunksjonaliteter for komplekse krav.
