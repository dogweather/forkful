---
title:                "Haskell: Wyszukiwanie i zamiana tekstu"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem musimy dokonać zmian w ogromnych plikach tekstowych. Może to być zmiana nazwy jednego słowa na inne, wykasowanie całych fragmentów tekstu lub zamiana wielu wystąpień jednego wyrażenia na inne. W takich przypadkach niezwykle pomocne jest narzędzie do wyszukiwania i zamiany tekstu. W tym artykule opowiemy o programowaniu tego typu narzędzia w języku Haskell.

## Jak to zrobić

Poniżej przedstawiamy przykłady kodu, które wyjaśnią jak dokonywać wyszukiwania i zamiany tekstu w języku Haskell. Kod będzie zapisany w blokach kodu ```Haskell```, aby zachować czytelność.

```
-- Importujemy bibliotekę do pracy z wyrażeniami regularnymi
import Text.RegularExpressions

-- Definujemy funkcję, która dokonuje wyszukiwania i zamiany tekstu
-- Pierwszy argument to wyrażenie regularne, które chcemy znaleźć
-- Drugi argument to tekst, w którym chcemy dokonać zmiany
-- Trzeci argument to wyrażenie, którem chcemy zastąpić znalezione wyrażenie
replace :: String -> String -> String -> String
replace regex input replacement =
  let match = input =~ regex :: Bool
  in if match then subRegex (mkRegex regex) input replacement else input
```

Kod ten używa biblioteki do pracy z wyrażeniami regularnymi i definiuje funkcję ```replace```, która przyjmuje trzy argumenty: wyrażenie regularne, tekst i wyrażenie, które ma zastąpić znalezione wyrażenie. W linii kodu zaczynającej się od ```in``` wykorzystujemy funkcję ```subRegex```, która wykonuje faktyczną zamianę tekstu na podstawie podanego wyrażenia regularnego oraz wyrażenia zastępczego.

```
> replace "Haskell" "Ja lubię programować w Haskell!" "Jestem zafascynowany programowaniem w Haskell!"
"Ja lubię programować Jestem zafascynowany programowaniem w Haskell!"
```

W powyższym przykładzie używamy naszej funkcji, aby znaleźć i zastąpić wszystkie wystąpienia słowa "Haskell" w danym tekście. Widzimy, że dokonuje ona zamiany tylko na pierwszym wystąpieniu, zgodnie z oczekiwaniami.

## Deep Dive

Praca ze zgłębianiem wyrażeń regularnych może być trudna i wymaga dużo praktyki. Jednak w języku Haskell istnieje wiele bibliotek, takich jak ```Text.RegularExpressions```, które ułatwiają pracę z wyrażeniami regularnymi. Warto zaznajomić się z dokumentacją tych bibliotek i eksperymentować z różnymi wyrażeniami, aby lepiej zrozumieć ich działanie.

## Zobacz też

Jeśli praca z wyrażeniami regularnymi w języku Haskell wciąż wydaje się trudna, warto sięgnąć po inne źródła, takie jak:

- [Dokumentacja biblioteki "Text.RegularExpressions"](https://hackage.haskell.org/package/regex-posix/docs/Text-RegularExpressions-Regex-Posix.html)
- [Przewodnik po wyrażeniach regularnych w Haskellu](https://wiki.haskell.org/Regular_expressions)
- [Książka "Real World Haskell" - rozdział o wyrażeniach regularnych](http://book.realworldhaskell.org/read/regular-expressions.html)

Dzięki tym źródłom z pewnością zrozumiesz lepiej jak działa wyszukiwanie i zamiana tekstu w języku Haskell.