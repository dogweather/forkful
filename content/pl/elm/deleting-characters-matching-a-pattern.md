---
title:                "Elm: Usuwanie znaków pasujących do wzoru"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego?

Czasem w programowaniu może zdarzyć się sytuacja, gdzie musimy usunąć pewne znaki ze stringa, które pasują do określonego wzorca. Może to być potrzebne w celu uproszczenia tekstu lub przetworzenia danych. W tym artykule opowiecie o tym, jak w języku Elm można usunąć znaki odpowiadające pewnemu wzorcowi.

## Jak to zrobić?

Zacznijmy od wyjaśnienia podstawowej składni języka Elm. Istnieje wiele różnych sposobów na obsługę stringów, ale my skupimy się na funkcji `replace` z biblioteki `String`. Oto przykładowy kod w Elm, który usuwa wszystkie wystąpienia małych liter "a":

```Elm
import String

text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
modifiedText = String.replace "a" "" text -- wynik: Lorem ipsum dolor sit met, consectetur discing elit.
```

Funkcja `replace` przyjmuje trzy argumenty: szukaną frazę, zastępowaną frazę i string, na którym ma być wykonywana operacja. W naszym przypadku zamieniamy "a" na pustego stringa, co skutkuje usunięciem wszystkich wystąpień. Można również użyć więcej niż jednej funkcji `replace`, aby usunąć więcej niż jeden znak.

## Deep Dive

Ale co jeśli chcemy usunąć nie tylko pojedynczy znak, ale całe wyrazy lub fragmenty tekstu? W takim przypadku możemy skorzystać z wyrażeń regularnych. Elm posiada wbudowaną bibliotekę `Regex`, która umożliwia obsługę wyrażeń regularnych. Przykładowy kod używający wyrażeń regularnych wyglądałby tak:

```Elm
import Regex

text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
modifiedText = Regex.replace Regex.All (Regex.regex "\\w+") (\_ -> "") text -- wynik: , . 
```

W tym przypadku wyrażenie regularne `\\w+` oznacza, że szukamy każdego wyrazu w stringu. Następnie, przy użyciu funkcji `replace` z wyrażeniami regularnymi, zamieniamy każdy wyraz na pusty string. W ten sposób usuwamy cały tekst, pozostawiając tylko znaki interpunkcyjne.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o języku Elm i jego funkcjonalności, warto przejrzeć inne artykuły na ten temat:

- Oficjalna dokumentacja języka Elm: https://guide.elm-lang.org/
- Wprowadzenie do programowania funkcyjnego w Elm: https://medium.com/@jaskiratr/understanding-functional-programming-with-elm-5b9752c462b8
- Przykłady wykorzystania Elm w praktyce: https://elmprogramming.com/