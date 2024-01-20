---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Wyszukiwanie długości łańcucha to operacja, która pozwala nam dowiedzieć się, ile znaków zawiera dany łańcuch. Programiści stosują ją, aby kontrolować i manipulować danymi tekstowymi w ich kodzie.

## Jak to zrobić:

W Haskellu, długość łańcucha można znaleźć za pomocą funkcji `length`. Oto przykład:

```Haskell
main = do
    let str = "Programowanie w Haskellu"
    print (length str)
```

Uruchomienie powyższego kodu zwróci:

```Haskell
25
```

## Głębszy wgląd:

(1) W kontekście historycznym, wiele języków programowania, wliczając Haskell, zaczynało z prostej funkcji do zliczania długości łańcucha, ale po pewnym czasie zaczęła pojawiać się potrzeba bardziej zaawansowanych funkcji do manipulacji łańcuchami.

(2) Alternatywą dla funkcji `length` w Haskellu jest użycie funkcji `foldr`:

```Haskell
countLength str = foldr (\_ n -> 1 + n) 0 str
```

(3) Implementacja `length` używa techniki rekurencji. Funkcja przechodzi przez łańcuch od początku do końca, zwiększając licznik przy każdym znaku.

## Zobacz też:

Haskell/Lists and tuples - https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples

Haskell/String processing - https://en.wikibooks.org/wiki/Haskell/String_processing

HaskellWiki: The implementation of length - https://wiki.haskell.org/The_implementation_of_length