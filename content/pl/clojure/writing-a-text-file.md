---
title:    "Clojure: Pisanie pliku tekstowego"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest ważnym elementem programowania w języku Clojure. Płynne operowanie na plikach tekstowych pozwala na łatwe przetwarzanie i analizę danych. Dodatkowo, tworzenie plików tekstowych jest często wykorzystywane do komunikacji z innymi programami.

## Jak to zrobić

Aby zapisać treść do pliku tekstowego, możemy skorzystać z funkcji `spit` w następujący sposób:

```Clojure
(spit "tekst.txt" "To jest przykładowy tekst.")
```

Powyższy kod utworzy plik "tekst.txt" i wypełni go tekstem "To jest przykładowy tekst." W przypadku, gdy chcemy dopisywać tekst do istniejącego już pliku, możemy użyć funkcji `spit` wraz z flagą `:append`:

```Clojure
(spit "tekst.txt" "To jest kolejny tekst." :append)
```

Teraz nasz plik "tekst.txt" zawiera oba teksty.

Możemy także odczytywać dane z pliku tekstowego przy użyciu funkcji `slurp`:

```Clojure
(slurp "tekst.txt")
```

Powyższa funkcja zwróci nam zawartość pliku jako string.

## Wnikliwa analiza

Clojure oferuje również bardziej zaawansowane możliwości manipulacji plikami. Na przykład, możemy użyć funkcji `file-seq`, aby stworzyć sekwencję plików w danym katalogu:

```Clojure
(file-seq (io/file "katalog"))
```

Powyższy przykład zwróci nam sekwencję zawierającą wszystkie pliki w folderze "katalog". Możemy także wykorzystać funkcję `file?`, aby sprawdzić, czy dany plik jest plikiem tekstowym:

```Clojure
(file? (io/file "tekst.txt"))
```

Ostatnią funkcją, którą warto poznać, jest `file-lines`, która zwraca sekwencję wierszy z pliku tekstowego:

```Clojure
(file-lines "tekst.txt")
```

Teraz możemy łatwo przejść przez wszystkie linie z pliku i dokonywać na nich różnych operacji.

## Zobacz też

- Dokumentacja Clojure dotycząca obsługi plików (https://clojuredocs.org/clojure.core/spit)
- Przykładowe projekty wykorzystujące ładowanie i zapisywanie danych do plików tekstowych (https://github.com/search?q=clojure+file+handling&type=Repositories)