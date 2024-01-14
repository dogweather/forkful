---
title:                "Elm: Wycinanie podciągów"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym wpisie na blogu dowiesz się, dlaczego warto wykorzystywać funkcję wyciągania podłańcuchów w języku Elm. Poznasz również przykłady kodu oraz przeczytasz o szczegółach tej operacji.

## Jak to zrobić

Funkcja extractSubstring w języku Elm pozwala na wyodrębnienie części ciągu znaków z danej wartości. Przykładowo, jeśli masz ciąg znaków "Hello World", a chcesz wyodrębnić tylko wyraz "Hello", możesz użyć funkcji extractSubstring w ten sposób:

```Elm
extractSubstring 0 5 "Hello World" -- wynik: "Hello"
```

W powyższym przykładzie pierwszym argumentem jest indeks początkowy, czyli od którego znaku ma zostać wyodrębiony podłańcuch. W tym przypadku jest to 0, ponieważ wyraz "Hello" rozpoczyna się od pierwszego znaku. Drugim argumentem jest długość podłańcucha, czyli ile znaków będzie wyodrębnionych od indeksu początkowego. W tym przypadku jest to 5, ponieważ wyraz "Hello" składa się z pięciu znaków.

Możesz również użyć ujemnych indeksów, aby wyodrębnić podłańcuch od końca ciągu znaków. Na przykład, jeśli chcesz wyodrębnić wyraz "World" z ciągu "Hello World", możesz użyć funkcji w ten sposób:

```Elm
extractSubstring -5 5 "Hello World" -- wynik: "World"
```

Jeśli nie podasz drugiego argumentu, zostanie wyodrębniony cały podłańcuch od danego indeksu do końca ciągu. Na przykład:

```Elm
extractSubstring 3 "Hello World" -- wynik: "lo World"
```

### Przykładowe użycie

Funkcja extractSubstring jest szczególnie przydatna w przypadku analizowania tekstu lub wyodrębniania konkretnych danych z ciągów znaków. Możesz używać jej do manipulowania danymi wejściowymi lub do tworzenia wyświetlanych treści w Twojej aplikacji. Przykładowo, jeśli w Twoim tekście wyświetlasz tylko fragment wiadomości, możesz użyć funkcji extractSubstring, aby wyświetlić tylko pierwszą część wiadomości.

## Dogłębna analiza

W przypadku niektórych operacji na ciągach znaków, takich jak funkcja replace, należy najpierw wyodrębnić podłańcuch, a następnie dokonać modyfikacji. W przypadku, gdy zależy Ci na wykorzystaniu wskaźnika początkowego and długości zamiast indeksu końcowego, możesz użyć funkcji extractSubstring, aby wygodnie odwoływać się do konkretnych części tekstu.

## Zobacz także

- Oficjalna dokumentacja funkcji extractSubstring w języku Elm: [https://package.elm-lang.org/packages/elm/string/latest/String#extractSubstring](https://package.elm-lang.org/packages/elm/string/latest/String#extractSubstring)
- Przykłady użycia funkcji extractSubstring: [https://elmprogramming.com/elm-string-extractsubstring.html](https://elmprogramming.com/elm-string-extractsubstring.html)
- Wpływ funkcji extractSubstring na wydajność aplikacji: [https://matteofigus.github.io/EasyLearnJS/js/2009/07/15/drew-representants-stickman-in-javascript-with-canvas-and-web-sockets/](https://matteofigus.github.io/EasyLearnJS/js/2009/07/15/drew-representants-stickman-in-javascript-with-canvas-and-web-sockets/)