---
title:    "Clojure: Sprawdzanie istnienia katalogu"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie istnienia katalogu jest ważnym elementem programowania w języku Clojure. Jeśli tworzysz aplikację, która musi mieć dostęp do określonych danych w określonym miejscu, konieczne jest sprawdzenie, czy dany katalog istnieje. W ten sposób można zapewnić, że aplikacja działa zgodnie z oczekiwaniami i uniknąć potencjalnych błędów.

## Jak to zrobić

```Clojure
(require '[clojure.java.io :as io])

(defn check-directory [directory]
  "Funkcja sprawdzająca istnienie katalogu.
  Zwraca true, jeśli katalog istnieje, a false w przeciwnym razie."
  (io/file directory) ;tworzy obiekt katalogu
  (.exists (io/file directory))) ;sprawdza istnienie katalogu i zwraca wartość logiczną

(check-directory "ścieżka/do/katalogu") ;wywołanie funkcji
;=> true
(check-directory "nieistniecy/katalog") ;wywołanie funkcji dla nieistniejącego katalogu
;=> false
```

## Głębsze zanurzenie

Możesz również wykorzystać funkcję `clojure.java.io/file-exists?`, która również sprawdza istnienie katalogu, ale zwraca wyjątek `FileNotFoundException` w przypadku nieistniejącego katalogu.

Innym przydatnym narzędziem jest funkcja `file-seq`, która przegląda wszystkie pliki i podkatalogi w danym katalogu, zwracając ich odpowiadające im obiekty. Możesz wykorzystać tę funkcję, aby przeiterować przez zawartość katalogu i wykonać operacje na poszczególnych plikach.

## Zobacz także

- Dokumentacja `clojure.java.io`: https://clojure.github.io/clojure/clojure.java.io-api.html
- Tutoriale o Clojure: https://www.learn-clojure.com/