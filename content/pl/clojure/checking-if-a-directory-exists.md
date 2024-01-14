---
title:                "Clojure: Sprawdzanie istnienia katalogu"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy sprawdzić, czy dany katalog istnieje przed wykonaniem pewnych operacji na plikach wewnątrz niego. Jest to ważne, ponieważ jeśli katalog nie istnieje, nasz program może generować błędy lub nie działać prawidłowo. W tym wpisie dowiesz się, jak w łatwy sposób sprawdzić istnienie katalogu w Clojure.

## Jak to zrobić

Sprawdzenie istnienia katalogu w Clojure jest bardzo proste. Wystarczy użyć funkcji `clojure.java.io/file` do utworzenia obiektu katalogu, a następnie użyć funkcji `exists?` do sprawdzenia czy istnieje. Poniżej znajduje się przykładowy kod:

```Clojure
(require '[clojure.java.io :as io])
(def directory (io/file "/ścieżka/do/katalogu"))
(exists? directory)
```

W powyższym przykładzie używamy funkcji `exists?` do sprawdzenia czy katalog istnieje. Funkcja ta zwraca `true` jeśli istnieje, lub `false` jeśli nie istnieje. Możemy również użyć tej funkcji do sprawdzania istnienia plików.

## Deep Dive

Podczas wywoływania funkcji `exists?`, Clojure wywołuje pod spodem funkcję `java.io.File#exists` z języka Java. Zwraca ona również `true` lub `false` w zależności od istnienia pliku lub katalogu. Jest to wygodne, ponieważ nie musimy importować dodatkowych bibliotek do operacji na plikach.

Jeśli chcesz dowiedzieć się więcej o funkcjach dostępnych w Clojure do pracy z plikami i katalogami, polecam przeczytać dokumentację Clojure na temat funkcji `clojure.java.io/file` i `exists?`.

## Zobacz także

- [Dokumentacja Clojure o pracy z plikami i katalogami](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Poradnik na temat operacji na plikach w Clojure](https://medium.com/@gonewest818/working-with-files-in-clojure-60ee19a594e4)
- [Funkcja exists? na stronie ClojureDocs](https://clojuredocs.org/clojure.java.io/exists_q)