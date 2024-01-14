---
title:                "Elm: Ekstrahowanie podciągów"
simple_title:         "Ekstrahowanie podciągów"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Wyodrębnianie podciągów jest ważnym aspektem programowania, ponieważ pozwala na manipulowanie i przetwarzanie tekstu w bardziej precyzyjny sposób. Jest to niezbędne w wielu aplikacjach, w których musimy przeprowadzić operacje na fragmentach tekstu, takich jak wyszukiwanie, zamiana czy analiza.

## Jak to zrobić?

Wykorzystując język Elm, możemy wygodnie i efektywnie wyodrębnić podciągi za pomocą funkcji `slice`. Przykładowe wywołanie tej funkcji wygląda następująco:

```elm
slice 10 15 "Wyodrębnij podciąg" 

-- Output: "podciąg"
```

Funkcja `slice` przyjmuje trzy argumenty: początkowy i końcowy indeks wyodrębnianego podciągu oraz tekst, z którego chcemy wyodrębnić podciąg. W powyższym przykładzie, wyodrębniliśmy podciąg z tekstu "Wyodrębnij podciąg" rozpoczynający się od 10-tej pozycji i kończący na 15-tej. 

Język Elm oferuje również inne funkcje do wyodrębniania podciągów, takie jak `left`, `right` czy `mid`. Warto zaznaczyć, że indeksacja w Elm zaczyna się od 0, czyli pierwsza pozycja tekstu ma indeks 0, druga pozycja ma indeks 1 itd.

## Wnikliwa analiza

Aby lepiej zrozumieć, jak funkcja `slice` działa, warto przyjrzeć się jej implementacji. W Elm, wszystkie funkcje są czyste, co oznacza, że zawsze zwracają tę samą wartość dla podanych argumentów. Implementacja funkcji `slice` wygląda następująco:

```elm
slice start stop string = 
    String.slice start stop string
```

Funkcja ta wykorzystuje moduł `String`, który dostarcza podstawowe operacje na tekstach, takie jak wyodrębnianie podciągów czy łączenie tekstów.

## Zobacz także

- Dokumentacja języka Elm dotycząca funkcji wyodrębniania podciągów: https://guide.elm-lang.org/strings.html#substring-functions
- Przykładowe kody wykorzystujące funkcję `slice` w Elm: https://ellie-app.com/3bKdWL6pTPha1
- Praktyczne zastosowania wyodrębniania podciągów w aplikacjach: https://www.dailydrip.com/topics/elm/drips/flutter-substring-manipulating-strings-in-elm