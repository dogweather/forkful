---
title:                "Elm: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery może być bardzo przydatna w wielu przypadkach, na przykład w filtracji i sortowaniu danych, porównywaniu ciągów znaków, czy w procesie walidacji danych. W tym blogu dowiesz się jak w prosty sposób przekonwertować dowolny ciąg znaków na małe litery w języku Elm.

## Jak to zrobić

Do konwertowania tekstu na małe litery w Elmie używamy wbudowanej funkcji `String.toLower`, która przyjmuje jako argument ciąg znaków i zwraca nowy ciąg znaków w formacie małych liter.

```elm
import String exposing (toLower)

name = "JAN"
lowerCaseName = toLower name

-- Output
lowerCaseName = "jan"
```

Funkcja `toLower` jest bardzo prosta w użyciu i idealnie sprawdza się w większości przypadków. Możemy ją również wykorzystać do konwertowania ciągów znaków pobieranych z pola input w formularzach oraz do pracy z danymi pobieranymi z zewnętrznych źródeł.

## Deep Dive

W niektórych przypadkach konwertowanie tekstu na małe litery może wymagać więcej uwagi. Na przykład, w języku polskim niektóre znaki diakrytyczne takie jak "ą" czy "ó" nie mają swoich odpowiedników w formacie małych liter. Dlatego w celu poprawnej konwersji tekstu zawierającego takie znaki, musimy najpierw wykorzystać funkcję `toLower` a następnie zastosować funkcję `String.normalize` aby zamienić polskie znaki na ich odpowiedniki w formacie małych liter.

```elm
import String exposing (toLower, normalize)

name = "Łukasz"
lowerCaseName = toLower name
normalizedName = normalize lowerCaseName

-- Output
normalizedName = "łukasz"
```

Zazwyczaj jednak nie musimy się martwić o ten szczegół, ponieważ najczęściej używane znaki diakrytyczne w języku polskim są obsługiwane przez funkcję `toLower`.

## Zobacz również

- Dokumentacja Elm: [String.toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Dokumentacja Elm: [String.normalize](https://package.elm-lang.org/packages/elm/core/latest/String#normalize)
- Poradnik Elm po polsku: [Programowanie funkcyjne w Elmie](https://programowanie.elm.pl/)