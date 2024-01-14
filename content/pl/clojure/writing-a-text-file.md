---
title:                "Clojure: Tworzenie pliku tekstowego"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego
Czemu warto napisać plik tekstowy w Clojure? To jedno z podstawowych narzędzi programowania i często używane w praktyce. Pisanie plików tekstowych może pomóc w tworzeniu stosunkowo prostych skryptów, a także w przenoszeniu danych pomiędzy różnymi aplikacjami.

## Jak to zrobić
Pisanie plików tekstowych w Clojure jest dość proste i polega na użyciu funkcji `spit`, która zapisuje dane do pliku. Oto przykładowy kod, który zapisuje listę liczb do pliku "numbers.txt":

```Clojure
(spit "numbers.txt" (str "(1 2 3 4 5)"))
```

Wykorzystując funkcję `spit` możemy także tworzyć pliki z dowolnym typem danych, takich jak np. słowniki czy listy zagnieżdżone. Poniżej znajduję się przykładowy kod z wykorzystaniem funkcji `spit` do zapisania słownika w pliku "dictionary.txt":

```Clojure
(spit "dictionary.txt" (str "{:name \"John\", :age 30, :country \"Poland\"}"))
```

Po wykonaniu powyższych kodów, w katalogu projektu powinny pojawić się nowo utworzone pliki z odpowiednimi danymi.

## Wgląd w temat
W Clojure istnieje również funkcja `slurp`, która pozwala na odczytanie zawartości pliku tekstowego do zmiennej. Jest to przydatne, gdy chcemy przetworzyć dane z pliku wewnątrz naszego programu. Oto przykładowy kod, który odczytuje dane z pliku "numbers.txt" i zapisuje je do zmiennej `data`:

```Clojure
(def data (slurp "numbers.txt"))
```

Możemy także manipulować zawartością pliku tekstowego poprzez wykorzystanie funkcji `subs` lub `split`, aby np. odczytać wybraną część tekstu lub podzielić tekst na listę.

## Zobacz także
- Dokumentacja Clojure dotycząca funkcji `spit`: https://clojuredocs.org/clojure.core/spit
- Polska społeczność Clojure: https://clojure.pl/