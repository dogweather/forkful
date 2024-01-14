---
title:    "Clojure: Zmiana wielkości liter w ciągu znaków"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Capitalizing w programowaniu to proces przekształcania pierwszej litery w słowie na wielką. Jest to przydatna umiejętność, gdy chcemy, aby nasze dane były w jednolitym formacie, np. przy generowaniu danych do raportów lub wyświetlaniu tytułów w aplikacji.

## Jak to zrobić

Za pomocą funkcji `clojure.string/capitalize` możemy łatwo przekształcić pierwszą literę w tekście na wielką. Poniżej przedstawiam przykładowy kod w języku Clojure:

```Clojure
(ns capitalization-example.core
  (:require [clojure.string :as str]))

(str/capitalize "hello world") ; Output: "Hello world"
(str/capitalize "1234test") ; Output: "1234test"
(str/capitalize "żółw") ; Output: "Żółw"
```

Jak widać, funkcja ta zachowuje się również poprawnie w przypadku liczb i znaków diakrytycznych.

## Głębsze zanurzenie

Funkcja `clojure.string/capitalize` działa poprawnie w większości przypadków, jednak istnieją pewne subtelności, na które warto zwrócić uwagę. Na przykład, jeśli chcemy zachować oryginalną wielkość liter w wyrazie, musimy zastosować funkcję `clojure.string/upper-case` lub `clojure.string/lower-case` w celu przekształcenia pozostałych liter. Ponadto, funkcja `clojure.string/capitalize` nie będzie poprawnie działać w przypadku, gdy wyraz zaczyna się od znaku specjalnego lub cyfry, w takim przypadku powinniśmy użyć innych funkcji, jak na przykład `clojure.string/replace-first`, aby naprawić błędne formatowanie.

## Zobacz też

- [Dokumentacja funkcji `clojure.string/capitalize`](https://clojuredocs.org/clojure.string/capitalize)
- [Inne przydatne funkcje z biblioteki `clojure.string`](https://clojuredocs.org/clojure.string)