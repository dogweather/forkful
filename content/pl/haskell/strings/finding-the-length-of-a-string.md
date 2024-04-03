---
date: 2024-01-20 17:47:32.655044-07:00
description: "Jak to zrobi\u0107: W Haskellu u\u017Cywamy funkcji `length` do znalezienia\
  \ d\u0142ugo\u015Bci stringa. Oto jak to wygl\u0105da."
lastmod: '2024-03-13T22:44:35.442222-06:00'
model: gpt-4-1106-preview
summary: "W Haskellu u\u017Cywamy funkcji `length` do znalezienia d\u0142ugo\u015B\
  ci stringa."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
