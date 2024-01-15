---
title:                "Używanie wyrażeń regularnych"
html_title:           "Elm: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego
 **Dlaczego warto używać wyrażeń regularnych w Elm** 

Wyrażenia regularne to bardzo przydatne narzędzie, które pozwala na skuteczne i szybkie przetwarzanie danych tekstowych. W Elm możemy wykorzystać je do wzorcowania i przeszukiwania tekstu w sposób bardzo wygodny i intuicyjny.

## Jak to robić
```Elm
-- Przykładowe wykorzystanie wyrażeń regularnych w Elm
-- Sprawdzenie czy dany tekst zawiera liczby
import Regex

tekst = "Hello world 123"
expression = Regex.regex "[0-9]+"
isNumber = Regex.contains expression tekst

-- isNumber będzie teraz równy True
```

W powyższym przykładzie zaimportowaliśmy moduł Regex, który umożliwia nam wykorzystanie wyrażeń regularnych. Następnie stworzyliśmy zmienną tekst, zawierającą przykładowy tekst do przeszukania. Kolejnym krokiem było utworzenie wyrażenia, które będzie szukało liczb w tekście (wzorzec [0-9]+ oznacza, że szukamy jednej lub więcej wystąpień cyfr). Na koniec wywołaliśmy funckję contains, która sprawdza czy wyrażenie pasuje do podanego tekstu. 

## Głębszy zanurzenie

Wyrażenia regularne pozwalają nam na jeszcze większą kontrolę nad przetwarzaniem tekstu w Elm. Możemy wykorzystać je do wyłapywania i wycinania konkretnych fragmentów tekstu, modyfikacji go lub nawet walidacji wprowadzonych przez użytkownika danych. 

W Elm wyrażenia regularne występują jako moduł Regex i udostępniają nam różne funkcje, takie jak:

- **contains:** służy do sprawdzenia czy wyrażenie pasuje do tekstu
- **find:** służy do znalezienia pierwszego pasującego fragmentu tekstu
- **matches:** zwraca listę wszystkich pasujących fragmentów tekstu
- **replace:** pozwala na zamianę pasujących fragmentów tekstu na inne wyrażenie lub tekst

Możemy także wykorzystać funkcję Regex.custom, która pozwala na stworzenie własnego wyrażenia regularnego z użyciem składni RegExp.

## Zobacz również
- Dokumentacja wyrażeń regularnych w Elm: https://package.elm-lang.org/packages/elm/regex/latest/
- Praktyczne przykłady wykorzystania wyrażeń regularnych w Elm: https://thoughtbot.com/blog/practical-regular-expressions-in-elm