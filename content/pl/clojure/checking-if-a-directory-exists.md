---
title:                "Sprawdzanie, czy istnieje katalog."
html_title:           "Clojure: Sprawdzanie, czy istnieje katalog."
simple_title:         "Sprawdzanie, czy istnieje katalog."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Sprawdzanie, czy istnieje katalog, jest ważną częścią programowania w Clojure. Pozwala to na sprawdzenie, czy dany katalog istnieje w systemie plików i umożliwia programistom wykonanie odpowiednich działań, w zależności od wyniku sprawdzenia. Jest to szczególnie przydatne, gdy program musi operować na różnych katalogach i plikach.

## Jak to zrobić:
```Clojure
(if (.exists (java.io.File. "/sciezka/do/katalogu"))
  (println "Katalog istnieje!")
  (println "Katalog nie istnieje!")
)
```

Przykładowe wyjście:
```
Katalog istnieje!
```

## Głębsza analiza:
Podczas tworzenia aplikacji w Clojure, często potrzebujemy sprawdzić, czy katalog istnieje przed wykonaniem określonych operacji na nim. Jest to szczególnie przydatne, gdy chcemy skopiować, przenieść lub usunąć katalog lub plik. Alternatywnym rozwiązaniem jest użycie funkcji ```clojure.java.io/file```, która zwraca obiekt pliku. Następnie możemy wykorzystać metodę ```.exists``` dla tego obiektu, aby sprawdzić, czy katalog istnieje. W implementacji, funkcja ta używa systemowej komendy ```stat``` do pobrania informacji o pliku lub katalogu.

## Zobacz też:
Dodatkowe informacje na temat sprawdzania istnienia katalogów w Clojure można znaleźć w oficjalnej dokumentacji: https://clojure.org/api/clojure.java.io/file.