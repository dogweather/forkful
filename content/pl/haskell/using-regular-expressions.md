---
title:                "Haskell: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## DlaczegoHaskell jest popularnym językiem programowania, który oferuje wiele narzędzi i bibliotek do pracy z danymi tekstowymi. Wyrażenia regularne są jednym z tych narzędzi, które pozwalają programistom przetwarzać, wyszukiwać i manipulować danymi tekstowymi w łatwy i wydajny sposób.

## Jak korzystać

Wyrażenia regularne w Haskellu są dostępne dzięki bibliotece `Text.Regex.Posix`, która oferuje dużo funkcji do pracy z wyrażeniami regularnymi. Aby rozpocząć, musimy najpierw zaimportować tę bibliotekę:

```Haskell
import Text.Regex.Posix
```

Aby wyszukać dopasowanie danego wzorca w tekście, możemy użyć funkcji `=~~`, która przyjmuje dwa argumenty: wyrażenie regularne oraz tekst, w którym szukamy dopasowania. Na przykład, chcemy znaleźć wszystkie numery telefonów w tekście:

```Haskell
"Moje numery telefonów to 123-456-789 i 987-654-321." =~~ "[0-9]{3}-[0-9]{3}-[0-9]{3}"
```

Output: `True`

Jeśli chcemy zwrócić konkretne dopasowanie, możemy użyć funkcji `=~`, która zwraca tuplę zaczynającą się od dopasowanego tekstu oraz kolejne grupy dopasowań. Na przykład, jeśli chcemy wyodrębnić tylko pierwszy numer telefonu:

```Haskell
"Moje numery telefonów to 123-456-789 i 987-654-321." =~ "[0-9]{3}-[0-9]{3}-[0-9]{3}"
```

Output: `("123-456-789", "")`

Możemy również użyć wyrażeń regularnych do zastępowania tekstu przy użyciu funkcji `=~~~`, która przyjmuje trzy argumenty: wyrażenie regularne, tekst, w którym chcemy coś zastąpić, oraz tekst zastępczy. Na przykład, jeśli chcemy zamienić wszystkie numery telefonów na "*numer telefonu zastępczy*":

```Haskell
"Moje numery telefonów to 123-456-789 i 987-654-321." =~~~ "[0-9]{3}-[0-9]{3}-[0-9]{3}" "*numer telefonu zastępczy*"
```

Output: `"Moje numery telefonów to *numer telefonu zastępczy* i *numer telefonu zastępczy*."`

## Głębsze zagadnienia

Wyrażenia regularne w Haskellu oferują wiele zaawansowanych funkcji, takich jak zaawansowane grupowanie, określanie liczby wystąpień lub odwołanie do poprzednich dopasowań.

Innym interesującym narzędziem jest moduł `Text.Regex.Applicative`, który oferuje parser wyrażeń regularnych, pozwalając na osadzanie wyrażeń w kodzie. Dzięki temu, możemy tworzyć bardziej złożone wyrażenia regularne bezpośrednio w kodzie.

## Zobacz także

1. [Dokumentacja Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
2. [Tutorial wyrażeń regularnych w Haskellu](https://wiki.haskell.org/Regular_expressions)
3. [Dokumentacja Text.Regex.Applicative](https://hackage.haskell.org/package/regex-applicative/docs/Text-Regex-Applicative.html)