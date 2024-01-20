---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co to jest i po co? 

Wydruk debugowania to prosta ale potężna technika pomagająca programistom zrozumieć, jak ich kod działa od środka. Poprzez wykorzystanie wydruku debugowania, można obserwować przepływ danych w kolejnych etapach działania programu.

## Jak To Zrobić:

Pierwszym krokiem jest importowanie namespace `clojure.tools.logging`, które umożliwia drukowanie na konsolę. Poniżej znajduje się przykład.

```clojure
(require '[clojure.tools.logging :as log])

(defn funckja-przykladowa [x]
  (log/debug "Wartość x: " x)
  (* x x))
```
Powyżej, funkcja `funckja-przykladowa` drukuje wartość `x` przed wykonaniem obliczeń. Wydruk pojawia się w konsoli podczas uruchamiania programu.

## Powiedzmy Więcej:

Kiedy mówimy o wydrukach debugowania, warto przypomnieć sobie o historii. Pomimo że istnieje wiele zaawansowanych narzędzi do debugowania, wydruki debugowania nadal są chętnie stosowane ze względu na swoj simplicytet.

Alternatywą do `clojure.tools.logging` mogą być inne biblioteki z dostępnością podobnej funkcjonalności, takie jak `Timbre` lub `tools.trace`.

Ważne jest jednak wspomnieć, że wydruki debugowania nie zastąpią dokładnego testowania! Powinniśmy traktować je jako dodatkowe narzędzie w naszym arsenale programistycznym.

## Zobacz Także:

- Clojure - podręcznik testowania: https://clojure.org/guides/testing
- Biblioteka Timbre: https://github.com/ptaoussanis/timbre
- Biblioteka tools.trace: https://github.com/clojure/tools.trace