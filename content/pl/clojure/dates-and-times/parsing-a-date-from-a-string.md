---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:51.694744-07:00
description: "Parsowanie daty z ci\u0105gu znak\xF3w w Clojure polega na przekszta\u0142\
  caniu tekstowych reprezentacji dat i czas\xF3w na bardziej u\u017Cyteczn\u0105 form\u0119\
  \ (np. obiekt DateTime\u2026"
lastmod: '2024-03-13T22:44:35.006287-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie daty z ci\u0105gu znak\xF3w w Clojure polega na przekszta\u0142\
  caniu tekstowych reprezentacji dat i czas\xF3w na bardziej u\u017Cyteczn\u0105 form\u0119\
  \ (np."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

## Jak to zrobić:
Clojure, będąc językiem JVM, pozwala bezpośrednio korzystać z bibliotek daty i czasu Javy. Zacznijmy od wbudowanej interoperacyjności z Javą, a następnie zobaczymy, jak wykorzystać popularną bibliotekę stron trzecich, clj-time, dla bardziej idiomatycznych rozwiązań w Clojure.

### Korzystanie z interop Java
Clojure może bezpośrednio wykorzystać `java.time.LocalDate` Javy do parsowania dat z ciągów znaków:
```clojure
(require '[clojure.java.io :as io])

; Parsowanie daty za pomocą interop Java
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; Wyjście: 2023-04-01
```

### Korzystanie z clj-time
Bardziej idiomatyczną biblioteką Clojure do pracy z datami i czasami jest `clj-time`. Owiń to Joda-Time, obszerną bibliotekę operacji na datach i czasach. Najpierw musisz dodać `clj-time` do zależności swojego projektu. Oto jak parsować ciąg daty używając `clj-time`:

```clojure
; Upewnij się, że dodałeś [clj-time "0.15.2"] do swojego project.clj pod :dependencies

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; Definiowanie formattera
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; Wyjście: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

Te przykłady demonstrują podstawowe parsowanie dat. Obydwie metody są użyteczne, ale `clj-time` może zapewnić bardziej zorientowane na Clojure podejście z dodatkowymi funkcjonalnościami dla złożonych wymagań.
