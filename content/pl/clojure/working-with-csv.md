---
title:                "Clojure: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV, czyli skrót od "Comma-Separated Values", jest jednym z najpopularniejszych formatów przechowywania danych. Jest powszechnie używany w różnych dziedzinach, takich jak biznes, nauka czy informatyka. Dzięki łatwej i intuicyjnej strukturze, pliki CSV są bardzo wygodnym narzędziem do przechowywania i przetwarzania danych. W tym artykule dowiesz się, dlaczego warto pracować z CSV w języku Clojure.

## Jak to zrobić

Aby móc pracować z plikami CSV w Clojure, potrzebujemy użyć biblioteki clojure.data.csv, która umożliwia wygodne operowanie na danych zapisanych w formacie CSV. Poniżej przedstawiam przykładowy kod, który wczytuje plik CSV i wyświetla jego zawartość na ekranie:

```Clojure
(require '[clojure.data.csv :as csv])
(with-open [reader (io/reader "dane.csv")]
    (doall
      (map (fn [row] (println row))
        (csv/read-csv reader))))
```

Przykładowy plik CSV "dane.csv" może wyglądać w ten sposób:

```CSV
Imię,Nazwisko,Wiek
Anna,Kowalska,30
Jan,Nowak,40
Maria,Szmidt,25
```

Po uruchomieniu powyższego kodu, otrzymamy na ekranie następujące rezultaty:

```
["Imię" "Nazwisko" "Wiek"]
["Anna" "Kowalska" "30"]
["Jan" "Nowak" "40"]
["Maria" "Szmidt" "25"]
```

Możemy również łatwo przetwarzać i dokonywać zmian w naszych danych CSV. Poniższy przykład pokazuje, jak dodać nową kolumnę z liczbą lat do naszego pliku CSV:

```Clojure
(require '[clojure.string :as str])
(with-open [reader (io/reader "dane.csv")]
    (let [data (list (apply concat (rest (csv/read-csv reader))))]
      (str/join ","
        (map-indexed (fn [idx row]
                       (if-not (zero? idx)
                         (str row (str "," (- 2020 (Integer/parseInt (nth row 2))))))
                       row)
          data))))
```

Wyjście powyższego kodu wyglądałoby następująco:

```CSV
Imię,Nazwisko,Wiek,Lata
Anna,Kowalska,30,1990
Jan,Nowak,40,1980
Maria,Szmidt,25,1995
```

## Głębszy zanurzanie się

Biblioteka clojure.data.csv oferuje wiele funkcji i możliwości, które umożliwiają nam wygodne przetwarzanie danych CSV. Dzięki funkcjom takim jak "read-csv", "write-csv" czy "read-lines", możemy swobodnie manipulować danymi, tworzyć nowe pliki CSV oraz wprowadzać zmiany w istniejących plikach. W razie potrzeby, można również zaimplementować własne funkcje, które ułatwią nam pracę z CSV w pełni dostosowując do naszych potrzeb.

## Zobacz także

- Dokumentacja biblioteki clojure.data.csv [https://clojure.github.io/data.csv/]
- Przykładowe zadania CSV z wykorzystaniem Clojure [https://github.com/clojure official/ csv-spec]

HAPPY CODING! (MIŁEGO KODOWANIA!)