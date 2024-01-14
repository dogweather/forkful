---
title:    "Elm: Korzystanie z wyrażeń regularnych"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Elm i potrzebujesz narzędzia do manipulowania i przetwarzania tekstu, to wyrażenia regularne są niezbędnym elementem w Twoim zestawie narzędzi. Pozwalają one na szybkie i precyzyjne porównywanie, wyszukiwanie i modyfikowanie tekstów, co znacznie ułatwia pracę w wielu projektach.

## Jak to zrobić

Aby zacząć korzystać z wyrażeń regularnych w Elm, musisz najpierw zaimportować moduł "Regex". Następnie możesz używać funkcji "Regex.find" lub "Regex.replace" z odpowiednimi parametrami, aby przetwarzać tekst według określonych wzorców.

```Elm
import Regex

-- znajdź wszystkie wystąpienia liczb w tekście
Regex.find (Regex.regex "\\d+") "Lorem ipsum 123 dolor sit amet." 
-- zwraca: [ (3,6) ] - indeksy początku i końca znalezionego wzorca

-- zamień wszystkie wystąpienia liczb na "X"
Regex.replace (Regex.regex "\\d+") (\\_ -> "X") "Lorem ipsum 123 dolor sit amet."
-- zwraca: "Lorem ipsum X dolor sit amet."
```

## Głębsze zagłębianie się

Wyrażenia regularne w Elm są oparte na silniku przeznaczonym dla języka JavaScript, co oznacza, że większość wyrażeń regularnych będzie działać podobnie w obu tych językach. Jednak Elm wykorzystuje statyczne typy, co pozwala na bardziej ścisłą i bezpieczną pracę z tekstami.

Szczegółowe informacje na temat wyrażeń regularnych w Elm można znaleźć w oficjalnej dokumentacji: https://package.elm-lang.org/packages/elm/regex/1.0.0/Regex

## Zobacz również

* https://medium.com/@dovalina/using-regular-expressions-in-elm-5ecbd757c8a6 - artykuł omawiający szczegółowo funkcje "Regex.find" i "Regex.replace"
* https://www.regextester.com/ - narzędzie online do testowania wyrażeń regularnych