---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "Clojure: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie czy istnieje katalog jest powszechnie wykonywaną operacją w wielu językach programowania. W Clojure istnieje wiele przydatnych funkcji, które ułatwiają ten proces. W tym artykule dowiesz się, dlaczego sprawdzanie czy istnieje katalog jest przydatne w programowaniu i jak to zrobić w Clojure.

## Jak to zrobić

Sprawdzenie czy istnieje katalog w Clojure jest bardzo proste. Wystarczy użyć funkcji ```file-seq```, która zwraca sekwencję plików i katalogów w danym katalogu. Następnie można wykorzystać funkcję ```some``` do sprawdzenia czy dany katalog jest na liście.

```Clojure
(file-seq "ścieżka/do/katalogu")
=> ("/ścieżka/do/katalogu/plik1.txt" "/ścieżka/do/katalogu/katalog1" "/ścieżka/do/katalogu/plik2.txt")

(some #"katalog1" (file-seq "ścieżka/do/katalogu"))
=> true
```

Jeśli katalog nie istnieje, funkcja ```file-seq``` zwróci pustą sekwencję, a funkcja ```some``` zwróci wartość ```false```.

```Clojure
(file-seq "nieistniejący/katalog")
=> ()

(some #"katalog1" (file-seq "nieistniejący/katalog"))
=> false
```

Sprawdzenie czy dany katalog jest podkatalogiem innego katalogu można wykonać przy użyciu funkcji ```subpath?```.

```Clojure
(subpath? "ścieżka/do/katalogu" "ścieżka/do/katalogu/katalog1")
=> true
```

## Deep Dive
Funkcje ```file-seq``` i ```subpath?``` korzystają z języka Java, aby wykonać operacje na plikach i katalogach. W przypadku funkcji ```file-seq```, korzysta z ```java.io.File``` do pobrania listy plików i katalogów w danym katalogu. Natomiast funkcja ```subpath?``` korzysta z metody ```java.nio.file.Path.subpath()```, która zwraca część ścieżki odpowiadającą podkatalogowi.

Warto zauważyć, że funkcje te nie sprawdzają poprawności ścieżki, ale jedynie czy istniejące pliki i katalogi pasują do podanego wzorca.

## Zobacz również
- Dokumentacja Clojure: https://clojuredocs.org/clojure.core/file-seq
- Poradnik programowania w Clojure: https://www.braveclojure.com/functional-programming-in-clojure/