---
title:                "Stosowanie wyrażeń regularnych"
html_title:           "Haskell: Stosowanie wyrażeń regularnych"
simple_title:         "Stosowanie wyrażeń regularnych"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia są bardzo przydatnym narzędziem w programowaniu, pozwalającym na efektywne przetwarzanie i manipulację tekstowymi danymi. Dzięki nim możemy wykonać skomplikowane operacje na ciągach znaków w prosty i intuicyjny sposób. W Haskellu regularne wyrażenia są dostępne dzięki modułowi "Text.Regex.PCRE" i znajomość ich wykorzystania może znacznie ułatwić pracę z tekstem w naszych programach.

## Jak to zrobić

Aby skorzystać z regularnych wyrażeń w Haskellu, musimy najpierw zaimportować odpowiedni moduł:
```Haskell
import Text.Regex.PCRE
```
Następnie możemy użyć funkcji `=~`, aby przetestować, czy dany ciąg znaków pasuje do danego wyrażenia regularnego. Na przykład:
```Haskell
"Hello world" =~ "world" :: Bool
-- Zwraca True
```
Możemy również wykorzystać wyrażenia regularne do wyciągania określonych fragmentów tekstu. W tym celu używamy funkcji `=~~`, która zwraca listę dopasowanych grup. Na przykład:
```Haskell
"Hello world" =~ "wo([a-z]+)" :: [[String]]
-- Zwraca [["world","rld"]]
```
Klauzulę ":: [[String]]" dodajemy, ponieważ wynik funkcji `=~~` jest listą list ciągów znaków.

Wyrażenia regularne pozwalają również na wykonywanie bardziej skomplikowanych operacji, takich jak zastępowanie wybranych fragmentów tekstu innymi wartościami. W tym celu używamy funkcji `subRegex`. Przykładowo:
```Haskell
subRegex (mkRegex "l+") "Hello world" "!"
-- Zwraca "He!o world"
```

## Głębszy zanurzenie

W Haskellu, wyrażenia regularne są wyrażeniami będącymi instancjami klasy typów `RegexLike`. Oznacza to, że możemy używać wielu funkcji, takich jak `subRegex` czy `match`, do manipulacji i analizy wyrażeń regularnych. Więcej informacji na ten temat można znaleźć w dokumentacji modułu "Text.Regex.PCRE" oraz w oficjalnej dokumentacji języka Haskell.

## Zobacz też

- [Dokumentacja modułu Text.Regex.PCRE](https://hackage.haskell.org/package/regex-pcre/docs/Text-Regex-PCRE.html)
- [Dokumentacja języka Haskell](https://www.haskell.org/documentation)