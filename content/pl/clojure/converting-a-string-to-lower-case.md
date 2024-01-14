---
title:                "Clojure: Konwersja ciągu znaków na małe litery"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Dlaczego

Konwertowanie ciągów znaków na mniejsze litery jest podstawową funkcją w każdym języku programowania, która jest niezbędna przy przetwarzaniu tekstu. W Clojure istnieje wiele sposobów na wykonanie tego zadania, co sprawia, że jest to bardzo elastyczne i użyteczne narzędzie dla programistów.

# Jak to zrobić

Zacznijmy od prostego przykładu, w którym zamienimy ciąg znaków "Hello, World!" na mniejsze litery.

```Clojure
(.toLowerCase "Hello, World!")
```

Output: "hello, world!"

Możemy również użyć funkcji ```clojure.string/lower-case``` , która sprowadza ciągi znaków do samej postaci "hello, world!".

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!")
```

Output: "hello, world!"

Jeśli chcemy zachować oryginalną formę tekstu, ale jedynie zamienić litery na mniejsze, możemy użyć funkcji ```clojure.string/capitalize``` , która zamieni tylko pierwszą literę na wielką.

```Clojure
(str/capitalize "hello, world!")
```

Output: "Hello, world!"

# Głębsza analiza

W Clojure istnieje wiele funkcji i metod, które pozwalają na konwertowanie ciągów znaków na mniejsze litery lub zamianę liter na wielkie. Warto zwrócić uwagę na to, że funkcje te są często wykorzystywane w połączeniu z innymi funkcjami, co umożliwia jeszcze bardziej zaawansowane operacje na tekście.

Możemy również spotkać się z różnymi problemami związanymi z konwertowaniem ciągów znaków, na przykład jeśli używamy znaków specjalnych lub innych języków. W takich przypadkach warto poszukać specjalnych funkcji lub rozwiązań, które pozwolą na prawidłową konwersję tekstu.

# Zobacz również

- [Funkcje tekstowe w Clojure](https://clojuredocs.org/clojure.string)
- [Przetwarzanie tekstu w Clojure](https://www.braveclojure.com/troubleshooting-strings-in-clojure/)