---
title:                "Tworzenie pliku tekstowego"
html_title:           "Clojure: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego może być przydatne w wielu sytuacjach podczas pisania kodu w Clojure. Może to być przydatne do zapisania danych do późniejszego wykorzystania lub do generowania róznych raportów i dokumentów.

## Jak To Zrobić

Aby zapisać plik tekstowy w Clojure, użyjemy funkcji "spit". Ta funkcja przyjmuje dwa argumenty: ścieżkę do pliku i dane, które chcemy zapisać. Przykładowy kod wyglądałby tak:

```Clojure
(spit "sciezka/do/pliku.txt" "To jest przykladowy tekst do zapisania")
```

Jeśli chcemy, aby tekst został dopisany do już istniejącego pliku, możemy użyć funkcji "spit-append":

```Clojure
(spit-append "sciezka/do/pliku.txt" "Ten tekst zostanie dopisany")
```

Możemy również wykorzystać funkcje "slurp" do odczytania danych z pliku tekstowego:

```Clojure
(slurp "sciezka/do/pliku.txt")
```

## Deep Dive

Funkcja "spit" domyślnie używa kodowania "UTF-8", ale można to zmienić, przekazując trzeci argument w postaci mapy:

```Clojure
(spit "sciezka/do/pliku.txt" "To będzie zapisane w UTF-16" {:encoding "UTF-16"})
```

Możemy również przekazywać funkcji "spit" sekwencje, a nie tylko pojedyncze wartości. W takim przypadku, każda wartość w sekwencji będzie zapisana w nowej linii w pliku.

## Zobacz również

- [Dokumentacja Clojure - funkcja spit](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/spit)
- [Dokumentacja Clojure - funkcja slurp](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/slurp)
- [Przewodnik po Clojure](https://clojure.org/guides)