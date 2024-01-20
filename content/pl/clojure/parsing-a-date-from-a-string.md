---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Parsowanie daty z łańcucha znaków to proces, dzięki któremu z ciągu tekstowego wydobywa się datę. Programiści robią to, aby przetwarzać daty zapisane jako tekst do formatu, który można łatwo manipulować w danym języku programowania.

# Jak to zrobić:

W Clojure, możemy parsować datę z łańcucha znaków za pomocą funkcji `parse` z biblioteki `clj-time.format`. Oto przykład:

```Clojure
(require '[clj-time.core :as tc])
(require '[clj-time.format :as tf])

(defn parse-date [s]
  (tf/parse (tf/formatters :basic-date) s))

(println (parse-date "20211010"))
```

Po uruchomieniu powyższego kodu, otrzymamy wynik:

```
2021-10-10T00:00:00.000Z
```

# Głębszy wgląd

Parsowanie daty z łańcucha znaków jest pojęciem tak starym, jak samo programowanie. Z biegiem lat pojawiło się wiele podejść do tego problemu. 

W Clojure, alternatywą dla `clj-time` może być `java.time` dostępne natywnie w JVM. Jako że Clojure działa na JVM, można korzystać z tej biblioteki bez dodatkowych zależności. 

Szczegół implementacji `clj-time` polega na wykorzystaniu biblioteki Joda-Time. `clj-time` to po prostu wrapper Clojure dla tej biblioteki. Joda-Time była dużym krokiem naprzód w porównaniu z klasą `java.util.Date` dostarczaną przez Java 1, ale obecnie zastępowana jest przez `java.time` we współczesnych wersjach Javy.

# Zobacz również

1. Dokumentacja `clj-time`: https://github.com/clj-time/clj-time
2. Dokumentacja `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
3. API Joda-Time: http://www.joda.org/joda-time/apidocs/index.html