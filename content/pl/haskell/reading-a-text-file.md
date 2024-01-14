---
title:    "Haskell: Odczytywanie pliku tekstowego"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto przeczytać plik tekstowy w Haskellu? To ważne pytanie, na które odpowiemy w tym artykule. Wraz z przykładami kodu i wyjścia programu, opiszemy jak można bez trudu odczytać plik tekstowy w języku Haskell.

## Jak

Podstawowym krokiem do odczytania pliku tekstowego w Haskellu jest wykorzystanie funkcji `readFile`, która odczytuje zawartość pliku i zwraca jego zawartość jako string.

```Haskell
main = do
    fileContents <- readFile "nazwa_pliku.txt"
    putStrLn fileContents
```

Powyższy kod próbuje odczytać plik o nazwie "nazwa_pliku.txt" i wyświetla jego zawartość na ekranie.

Możemy także wykorzystać funkcję `withFile`, która umożliwia bezpieczne odczytywanie pliku tekstowego. Dzięki temu, nie musimy martwić się o ewentualne problemy związane z otwieraniem lub zamykaniem pliku.

```Haskell
import System.IO

main = do
    withFile "nazwa_pliku.txt" ReadMode (\handle -> do
        fileContents <- hGetContents handle
        putStrLn fileContents)
```

Powyższy kod wykorzystuje `withFile`, aby otworzyć plik tekstowy w trybie "ReadMode", a następnie odczytuje jego zawartość za pomocą funkcji `hGetContents`. Na koniec, zawartość pliku jest wypisana na ekranie przy pomocy funkcji `putStrLn`.

## Głębsza przeprawa

Oprócz podstawowych funkcji, Haskell oferuje także wiele narzędzi i bibliotek, które ułatwiają odczytywanie plików tekstowych. Na przykład, biblioteka "text" oferuje funkcje do konwertowania danych tekstu na wartości liczbowe oraz do tokenizacji tekstu.

Inną przydatną biblioteką jest "split", która umożliwia dzielenie tekstu na części za pomocą określonego separatora. Może to być bardzo przydatne w przypadku przetwarzania plików CSV.

## Zobacz też

- [Dokumentacja funkcji `readFile`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:readFile)
- [Dokumentacja funkcji `withFile`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:withFile)
- [Biblioteka "text"](https://hackage.haskell.org/package/text)
- [Biblioteka "split"](https://hackage.haskell.org/package/split)