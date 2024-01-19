---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja stringów to proces podstawiania zmiennych bezpośrednio do stringów. Programiści korzystają z tego, aby dynamicznie manipulować tekstem i zwiększyć czytelność kodu.

## Jak to zrobić:

W Haskellu, do interpolacji stringów możemy użyć pakietu `Text.Printf`:

```Haskell
import Text.Printf

main = do
    let name = "Jan"
    let age = 25
    printf "Cześć %s, masz %d lat.\n" name age
```
Wynikowy output to:

```
Cześć Jan, masz 25 lat.
```

## Deep Dive

Interpolacja stringów jest podstawowym rozwiązaniem w wielu językach programowania, nie tylko w Haskellu. Powstała z chęci ułatwienia tworzenia skomplikowanych stringów.

Alternatywnie, w Haskellu możemy użyć operatora `(++)` do łączenia stringów, ale to nie jest tak czytelne jak interpolacja.

Co do realizacji, `Text.Printf` wykorzystuje technikę o nazwie "typy formatu", która pozwala na zrozumienie, jakie argumenty są oczekiwane i jaki typ jest zwracany na podstawie formatu stringu.

## Zobacz również:

- [Printf w Haskellu](https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Printf.html)
- [Interpolacja stringów w Haskellu](https://ro-che.info/articles/2017-03-26-haskell-string-interpolation)