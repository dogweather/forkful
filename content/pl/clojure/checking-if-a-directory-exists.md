---
title:                "Clojure: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy katalog istnieje, jest bardzo ważnym elementem programowania w Clojure. Pozwala to na upewnienie się, że nasz program będzie działał poprawnie i nie będzie wywoływał błędów związanych z brakującymi katalogami.

## Jak to zrobić

Sprawdzanie, czy katalog istnieje, może wydawać się trudne, ale w rzeczywistości jest to bardzo prosta czynność w języku Clojure. Wystarczy użyć funkcji "clojure.java.io/file" i podać ścieżkę do katalogu, który chcemy sprawdzić. Następnie wywołujemy funkcję "exists?" na stworzonym obiekcie, która zwróci wartość true, jeśli katalog istnieje, lub false, jeśli nie istnieje.

```Clojure
(def dir (clojure.java.io/file "/sciezka/do/katalogu"))
(exists? dir)
;; Output: false
```

W powyższym przykładzie stworzyliśmy zmienną "dir" przechowującą obiekt katalogu. Następnie wywołaliśmy funkcję "exists?" na tej zmiennej, która zwróciła wartość false, ponieważ katalog jeszcze nie istnieje.

Jednak warto pamiętać, że ta metoda nie jest w 100% niezawodna. Istnieje szansa, że katalog zostanie utworzony lub usunięty między wywołaniami funkcji. Dlatego ważne jest, aby również stosować odpowiednie obsługiwanie wyjątków, aby uniknąć błędów w programie.

## Dogłębna analiza

Podczas sprawdzania, czy katalog istnieje, warto również wiedzieć, że funkcja "exists?" działa dla dowolnego typu pliku, a nie tylko dla katalogów. Oznacza to, że można jej użyć również do sprawdzania istnienia plików lub innych zasobów.

Ponadto, jeśli chcemy uzyskać więcej informacji o pliku, na którym wywołujemy funkcję "exists?", można również wykorzystać funkcję "file-seq", która zwraca sekwencję plików i katalogów w podanej ścieżce. W ten sposób można uzyskać dostęp do dodatkowych informacji, takich jak nazwa pliku, rozmiar, data utworzenia itp.

## Zobacz również

- [Dokumentacja Clojure o funkcji "exists?"](https://clojuredocs.org/clojure.java.io/exists_q)
- [Poradnik o sprawdzaniu istnienia plików w Clojure](https://clojureunraveled.com/check-file-exists/)
- [Tutorial o obsłudze wyjątków w Clojure](https://purelyfunctional.tv/article/exception-handling-in-clojure/)