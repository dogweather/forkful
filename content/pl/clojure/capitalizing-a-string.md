---
title:                "Clojure: Zmiana pierwszej litery na wielką w ciągu znaków"
simple_title:         "Zmiana pierwszej litery na wielką w ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego string powinien być pisany z wielkiej litery? Jest to często stosowana praktyka w programowaniu, aby poprawić czytelność i jednolitą formę w kodzie. Dodatkowo, niektóre funkcje w języku Clojure, takie jak `capitalize`, wymagają, aby pierwsza litera danego stringu była wielka.

## Jak to zrobić

Kodowanie stringa z wielką literę w języku Clojure jest bardzo proste i może być wykonane na kilka różnych sposobów. Przykładowe wykorzystanie `capitalize` wygląda następująco:

```Clojure
(capitalize "witaj świecie") ; WYNIK: "Witaj świecie"
```

Można również użyć funkcji `str/split` i `str/join` aby wykorzystać bibliotekę `clojure.string` do zmiany pierwszej litery na wielką:

```Clojure
(require '[clojure.string :as str])

(-> "witaj świecie"
  (str/split #" ")
  (mapv #(str/capitalize %))
  (str/join " ")) ; WYNIK: "Witaj Świecie"
```

## Głębszy wgląd

W języku Clojure wielkość liter ma znaczenie, ponieważ jest to język funkcyjny i funkcje są niemutowalne. Dlatego też, jeśli próbujesz zmienić wielkość liter w istniejącym stringu, zostanie stworzona nowa kopia z pożądanymi zmianami, a oryginalny string pozostanie bez zmian.

Można również zauważyć, że funkcje `capitalize` i `str/capitalize` zwrócą string z pierwszą literą zmieniającą rozmiar w przypadku, gdy oryginalna litera jest mała, jednak nie zmieniają stringa, jeśli pierwsza litera jest już wielka. W takim przypadku można użyć funkcji `str/upper-case` aby zapewnić, że cały string będzie zapisany dużymi literami.

## Zobacz także

- [Dokumentacja języka Clojure dotycząca zmian liter](https://clojuredocs.org/clojure.string/capitalize)
- [Oficjalna strona języka Clojure](https://clojure.org/)
- [Poradnik dla początkujących w języku Clojure](https://learnxinyminutes.com/docs/pl-pl/clojure-pl/)