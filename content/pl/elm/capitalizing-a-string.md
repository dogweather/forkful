---
title:    "Elm: Zmiana wielkości liter ciągu znaków"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś początkującym programistą w Elm, prawdopodobnie słyszałeś o funkcji `String.capitalize`. Możesz zastanawiać się, po co jest potrzebna ta funkcja i kiedy powinieneś jej używać. W tym blogu dowiecie się więcej o tym, dlaczego warto używać funkcji `String.capitalize` w swoim kodzie.

## Jak to zrobić

Możesz wykorzystać funkcję `String.capitalize` w swoim kodzie, gdy chcesz zmienić pierwszą literę wyrazu na wielką. Przykładowo, jeśli masz string "szczekam", po użyciu funkcji `String.capitalize` otrzymasz "Szczekam". Możesz również zastosować tę funkcję do całych zdań, zmieniając pierwszą literę każdego wyrazu na wielką.

```Elm
String.capitalize "dziękuję" -- "Dziękuję"

String.capitalize "mam trzy psy." -- "Mam trzy psy."
```

Funkcja `String.capitalize` jest bardzo przydatna w przypadku, gdy chcesz poprawić wygląd tekstu w swoim programie lub wygenerować wyświetlane komunikaty.

## Głębsze zagadnienia

Warto pamiętać, że funkcja `String.capitalize` nie zmieni liter, które już są wielkie. Jeśli więc masz string zawierający już wielkie litery, funkcja ta nie wpłynie na nie w żaden sposób.

```Elm
String.capitalize "Hello, World!" -- "Hello, World!"
```

Możesz również zastosować funkcję `String.capitalize` do pustych stringów lub stringów zawierających tylko znaki interpunkcyjne, jednak nie spowoduje to żadnych zmian w wyjściowym tekście.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcji `String.capitalize` i innych przydatnych funkcjach w języku Elm, polecam zapoznać się z poniższymi artykułami:

- ["Język Elm dla początkujących" (ang.)](https://guide.elm-lang.org/)
- ["Jak używać funkcji w Elm" (ang.)](https://elmprogramming.com/functions.html)
- ["Pisanie funkcji w Elm" (ang.)](https://elmprogramming.com/functions.html)

Dziękujemy za przeczytanie naszego bloga. Mam nadzieję, że ten artykuł pozwolił Ci lepiej zrozumieć funkcję `String.capitalize` i jej zastosowanie w Elm. Do zobaczenia w kolejnych wpisach!