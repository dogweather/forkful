---
title:    "Clojure: Odczytywanie pliku tekstowego"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak czytać zawartość pliku tekstowego w języku Clojure? Może potrzebujesz przetworzyć duże ilości danych lub chcesz automatycznie pobierać informacje z zewnętrznych źródeł? Dowiedz się, jak możliwe jest to przy użyciu kilku prostych linii kodu w Clojure.

## Jak to zrobić

```Clojure
;; Otwórz plik o nazwie "dane.txt"
(with-open [plik (clojure.java.io/reader "dane.txt")]
  ;; Wyświetl zawartość pliku
  (println (slurp plik)))
```

W tym prostej linii kodu otwieramy plik tekstowy "dane.txt" przy użyciu funkcji `with-open`, która automatycznie zamyka plik po zakończeniu operacji. Następnie za pomocą funkcji `slurp` odczytujemy całą zawartość pliku i wypisujemy ją na ekranie. Wyobraź sobie teraz, że zamiast wypisania na ekran, możemy przetworzyć te dane i użyć ich w naszej aplikacji.

```Clojure
;; Otwórz plik o nazwie "dane.txt"
(with-open [plik (clojure.java.io/reader "dane.txt")]
  ;; Wczytaj linie z pliku do listy
  (let [linie (line-seq plik)]
    ;; Przetwórz każdą linię i wyświetl ją na ekranie
    (doseq [linia linie]
      (println linia))))
```

Tutaj otwieramy plik i używamy funkcji `line-seq`, która zwraca listę zawierającą każdą linię z pliku. Następnie za pomocą pętli `doseq`, przetwarzamy każdą linię i wyświetlamy ją na ekranie. Możemy również wykonać dowolne inne operacje na tych liniach, np. zapisanie ich do bazy danych lub utworzenie nowych plików.

## Głębszy wgląd

Oprócz standardowych funkcji takich jak `slurp` czy `line-seq`, istnieje wiele innych sposobów na czytanie plików tekstowych w Clojure. Funkcja `slurp` jest przydatna w przypadku małych plików, ale może mieć znaczący wpływ na wydajność, gdy przetwarzamy duże ilości danych. Dlatego warto rozważyć użycie funkcji `reader` lub `io/input-stream`, które pozwalają na bardziej elastyczne przetwarzanie plików wycinkami.

```Clojure
;; Utwórz strumień danych z pliku
(def plik (clojure.java.io/input-stream "dane.txt"))

;; Wczytaj dane do pamięci wycinkami
;; Pierwszy argument to maksymalna liczba bajtów do wczytania
;; Drugi argument to opcjonalnie można podać początkowy offset
;; Trzeci argument to opcjonalnie można podać maksymalną długość danych
;; W tym przykładzie wczytamy tylko pierwsze 10 bajtów
(def dane (.read plik 10))

;; Zamknij strumień danych
(.close plik)

;; Wypisz wczytane dane na ekranie
(println dane)
```

Funkcja `read` pozwala na wczytanie strumienia danych wycinkami, co może być przydatne, gdy nie chcemy wczytać całego pliku na raz do pamięci.

## Zobacz także

- [Dokumentacja funkcji `slurp`](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/slurp)
- [Dokumentacja funkcji `line-seq`](https://clojuredocs.org/clojure.core/line-seq)
- [Dokumentacja funkcji `with-open`](https://clojured