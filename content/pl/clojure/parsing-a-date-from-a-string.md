---
title:                "Analiza składniowa daty z łańcucha znaków"
date:                  2024-02-03T19:13:51.694744-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty z ciągu znaków w Clojure polega na przekształcaniu tekstowych reprezentacji dat i czasów na bardziej użyteczną formę (np. obiekt DateTime Clojure). Ten proces jest podstawą dla przetwarzania danych, logowania lub dowolnej aplikacji manipulującej danymi czasowymi, umożliwiając programistom wykonywanie operacji, porównań czy manipulacji na datach efektywnie.

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
