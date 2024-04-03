---
date: 2024-01-20 17:55:49.075368-07:00
description: "How to (Jak to zrobi\u0107): W Clojure, argumenty linii polece\u0144\
  \ s\u0105 dost\u0119pne jako lista string\xF3w `*command-line-args*`. Oto jak to\
  \ wygl\u0105da w praktyce."
lastmod: '2024-03-13T22:44:35.012519-06:00'
model: gpt-4-1106-preview
summary: "W Clojure, argumenty linii polece\u0144 s\u0105 dost\u0119pne jako lista\
  \ string\xF3w `*command-line-args*`."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## How to (Jak to zrobić):
W Clojure, argumenty linii poleceń są dostępne jako lista stringów `*command-line-args*`. Oto jak to wygląda w praktyce:

```Clojure
; Uruchomienie programu: lein run arg1 arg2

(defn -main
  [& args]
  (println "Argumenty: " args))

; Wynik:
; Argumenty:  (arg1 arg2)
```
Jeśli chcesz przetwarzać argumenty na dane numeryczne lub stosować flagi, użyj biblioteki third-party, np. `tools.cli`.

## Deep Dive (W głębię tematu):
Clojure, jako język na platformie JVM, obsługuje argumenty linii poleceń w sposób podobny do Javy. W przeszłości, opcje obsługi były ograniczone i wymagały ręcznego parsowania stringów. Dzisiaj, biblioteki takie jak `tools.cli` lub `clj-commons/cli` oferują bardziej zaawansowane i wygodne narzędzia do obsługi argumentów.

Alternatywne podejścia to np. definiowanie konfiguracji w plikach edn lub yaml, ale argumenty linii poleceń nadal są popularne ze względu na prostotę i bezpośredniość.

Szczegółowe implementacje różnią się zależnie od tego, czy aplikacja Clojure jest uruchamiana jako skrypt, graalvm native-image, czy standardowy jar, ale zasada działania pozostaje podobna.

## See Also (Zobacz także):
- Dokumentacja `tools.cli`: https://github.com/clojure/tools.cli
- Tutorial dotyczący argumentów linii poleceń w Clojure: https://clojure.org/guides/deps_and_cli
- Porównanie `tools.cli` z innymi bibliotekami: https://gist.github.com/dakrone/2147459
