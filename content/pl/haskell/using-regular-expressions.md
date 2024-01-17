---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Haskell: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co to jest wyrażenie regularne i dlaczego programiści tego potrzebują?

Wyrażenia regularne to wyrażenia lub wzorce służące do dopasowania i manipulowania tekstem w programowaniu. Programiści często używają wyrażeń regularnych w celu wykonywania operacji związanych z analizą lub przetwarzaniem danych tekstowych. Jest to szybka i skuteczna metoda, która pozwala na precyzyjne dopasowanie do wybranych wzorców w tekście.

## Jak to zrobić:

Wyrażenia regularne są dostępne w języku Haskell dzięki modułowi `Text.Regex.Posix`. Można użyć funkcji `=~` do dopasowywania wzorca do tekstu i funkcji `=~~` do globalnego dopasowania. Przykład kodu:

```Haskell
import Text.Regex.Posix

-- Dopasowanie wzorca "abc" do tekstu "Abcdef" zwróci True.
"Abcdef" =~ "abc" :: Bool 

-- Dopełnienie wzorca "y.*z" do tekstu "xyz" zwróci zamrożoną listę ["yz"].
"xyz" =~~ "y.*z" :: Maybe [String]
```

## Głębsze spojrzenie:

Wyrażenia regularne mają swoje korzenie w teorii automatów skończonych i są używane od kilku dziesięcioleci w programowaniu. Alternatywne podejścia do manipulacji tekstem to m.in. wykorzystanie funkcji wbudowanych w języki programowania lub wykorzystanie parserów do analizy składniowej. Implementacja wyrażeń regularnych w Haskellu jest oparta na bibliotece `regex-posix`, która jest zgodna z POSIX i oferuje rozszerzoną składnię dla bardziej zaawansowanych wyrażeń.

## Zobacz też:

🔗 [Oficjalna dokumentacja modułu `Text.Regex.Posix`](https://hackage.haskell.org/package/regex-posix)

🔗 [Tutorial o użyciu wyrażeń regularnych w Haskellu](https://www.tutorialspoint.com/haskell/haskell_regular_expressions.htm)