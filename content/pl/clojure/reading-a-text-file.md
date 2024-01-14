---
title:                "Clojure: Odczytywanie pliku tekstowego"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w języku Clojure przynosi wiele korzyści, w tym możliwość łatwego odczytywania plików tekstowych. W tym artykule dowiecie się, dlaczego warto poznać mechanizm odczytu plików tekstowych i jak to zrobić w języku Clojure. 

## Jak To Zrobić

Odczytywanie plików tekstowych w języku Clojure jest proste i wygodne. Wystarczy użyć funkcji `slurp`, która wczytuje zawartość całego pliku i zwraca ją jako ciąg znaków. Poniżej przedstawiamy przykładowy kod, który otwiera plik `text.txt` znajdujący się w tym samym folderze co plik z kodem. 

```Clojure
(defn read-file [file]
  (slurp file))

(read-file "text.txt")
```

W wyniku otrzymujemy zawartość pliku `text.txt` w postaci ciągu znaków. Jeśli chcemy przetworzyć zawartość pliku jako listę, możemy użyć funkcji `clojure.string/split-lines`, która dzieli ciąg znaków na poszczególne linie. 

```Clojure
(defn read-file-lines [file]
  (clojure.string/split-lines (slurp file)))

(read-file-lines "text.txt")
```

W przypadku, gdy plik zawiera dane w formacie CSV (Comma-Separated Values), możemy skorzystać z biblioteki `clojure-csv` i funkcji `clojure-csv.core/read-csv`, która zwraca listę list danych z pliku. 

```Clojure
(require '[clojure-csv.core :as csv])

(defn read-csv [file]
  (csv/read-csv file))

(read-csv "data.csv")
```

W powyższych przykładach zakładamy, że plik znajduje się w tym samym folderze co plik z kodem. Jeśli plik znajduje się w innym folderze, należy podać pełną ścieżkę do pliku. 

## Deep Dive

Gdy dany plik tekstowy jest otwarty, jest czytany jako strumień znaków, co oznacza, że jest od razu dostępny w programie jako ciąg znaków, a nie musimy wczytywać go do pamięci. W przypadku plików o bardzo dużej ilości danych, jest to znacząca zaleta, ponieważ unikamy w ten sposób lądowania na pamięci komputera. 

Ponadto, funkcja `slurp` domyślnie używa kodowania UTF-8, ale można też określić inne kodowanie poprzez dodanie kolejnego argumentu. 

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o czytaniu i przetwarzaniu plików w języku Clojure, polecamy zapoznać się z poniższymi linkami:

- Dokumentacja Clojure: https://clojure.org/api/cheatsheet#IO
- Przetwarzanie plików CSV w języku Clojure: https://clojure.com/blog/2013/06/28/csv.html
- Oficjalna strona biblioteki clojure-csv: https://github.com/clojure-csv/clojure-csv

Bądź na bieżąco z blogami i artykułami na temat języka Clojure, śledząc naszą firmę na Twitterze (@example) i czytając nasz blog na stronie www.example.pl.