---
title:    "Clojure: Dostęp do bieżącej daty"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego warto pisać w języku Clojure?

Jeśli jesteś programistą lub programistką i interesują cię języki programowania, z pewnością słyszałeś/aś już o języku Clojure. Jest to język funkcyjny, działający na wirtualnej maszynie Javy. Dzięki swojej prostocie i wydajności, cieszy się coraz większą popularnością wśród programistów, szczególnie w dziedzinie programowania funkcyjnego.

Ale dlaczego warto pisać w języku Clojure? Jedną z jego zalet jest możliwość tworzenia skalowalnych aplikacji, które łatwo utrzymać i rozwijać. Dodatkowo, Clojure oferuje bogate narzędzia i biblioteki, które ułatwiają pracę programistów. Jednym z popularnych zastosowań języka Clojure jest tworzenie aplikacji internetowych i systemów rozproszonych.

Jeśli chcesz poznać więcej o języku Clojure i spróbować swoich sił w programowaniu funkcyjnym, możesz zacząć od prostej, ale przydatnej operacji - pobierania aktualnej daty.

## Jak to zrobić w Clojure?

```Clojure
(ns read-date.core
  (:require [clj-time.core :as time])) ; importujemy bibliotekę clj-time

(defn get-current-date []
  (let [current-date (time/today)] ; tworzymy zmienną z aktualną datą
    (print current-date))) ; wyświetlamy datę w konsoli

(get-current-date) ; wywołujemy naszą funkcję
```

Output:

#inst "2021-09-22T00:00:00.000-00:00"

Jak widać, pobranie aktualnej daty w języku Clojure jest bardzo proste i wymaga tylko kilku linii kodu. Dzięki bibliotece clj-time, możemy w łatwy sposób manipulować i formatować datę według swoich potrzeb.

## Głębsze zanurzenie

Jeśli jesteś ciekawy/ciekawa, jak dokładnie funkcja (time/today) z biblioteki clj-time pobiera aktualną datę, możesz zajrzeć do jej źródłowego kodu. W bardziej zaawansowanym projekcie, warto również przeczytać więcej o obsłudze czasu w języku Clojure, np. wykorzystując bibliotekę java-time.

## Zobacz również

- [Oficjalna dokumentacja języka Clojure](https://clojure.org/)
- [Biblioteka clj-time](https://github.com/clj-time/clj-time)
- [Przewodnik dla początkujących w języku Clojure](https://clojure.org/guides/getting_started)