---
title:                "Znalezienie długości ciągu znaków"
aliases: - /pl/haskell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:32.655044-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Liczenie długości łańcucha to ustalenie, ile znaków zawiera tekst. Programiści robią to, żeby wiedzieć, jak dużo danych przetwarzają i by zarządzać nimi efektywnie w różnych kontekstach, jak formatowanie czy walidacja danych.

## Jak to zrobić:

W Haskellu używamy funkcji `length` do znalezienia długości stringa. Oto jak to wygląda:

```haskell
main :: IO ()
main = do
    let myString = "Witaj, świat!"
    print (length myString) -- Wyświetla długość stringa
```

Output:
```
13
```

## Zanurz się głębiej

Historia języków programowania jest pełna różnych metod liczenia znaków w stringach. W Haskellu funkcja `length` jest częścią standardowej biblioteki i działa na dowolnej liście, ponieważ stringi to po prostu listy znaków.

Inne sposoby? Można również użyć kombinatorów z biblioteki `Data.Text` dla wydajniejszej obsługi dużych tekstów, albo własnoręcznie przejść przez każdy znak używając rekurencji czy funkcji wyższego rzędu.

Ciekawostka: `length` w Haskellu jest typowo implementowana jako funkcja przejścia przez całą listę do końca, więc ma złożoność liniową O(n). Nie jest to najwydajniejsze dla wielkich list, ale w przypadku stringów zazwyczaj nie stanowi problemu.

## Zobacz również

- Dokumentacja funkcji `length`: [Hackage: base-4.16.0.0: Data.List](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html#v:length)
- Haskell Wiki o stringach: [HaskellWiki: String](https://wiki.haskell.org/String)
- Lepsze praktyki z `Data.Text`: [Hackage: text](https://hackage.haskell.org/package/text)
- Przykłady i tutorial Haskell: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
