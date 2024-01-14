---
title:    "Elm: Zamiana ciągu znaków na małe litery"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego?

Konwertowanie stringów na małe litery jest częstym zadaniem w programowaniu. Jest to przydatne w wielu przypadkach, na przykład gdy chcemy porównać dwa stringi bez uwzględniania wielkości liter. W języku Elm istnieje wbudowana funkcja `String.toLower`, która wykonuje dokładnie to. W tym artykule dowiesz się, jak ją wykorzystać.

## Jak to zrobić?

Aby skonwertować string na małe litery, wystarczy wywołać funkcję `String.toLower` na danym stringu. Na przykład:

```elm
String.toLower "WITAJ!" --> "witaj!"
```

Funkcja ta zwraca nowy string zawierający wszystkie litery w małej formie. Można ją również wykorzystać do konwersji pojedynczych liter:

```elm
String.toLower "A" --> "a"
```

Jeśli chcesz dodatkowo usunąć również akcenty ze stringa, możesz skorzystać z funkcji `String.normalize` przed użyciem `String.toLower`:

```elm
String.toLower (String.normalize "Źdźbło") --> "zdzblo"
```

Aby sprawdzić, czy dwa stringi są równe bez względu na wielkość liter, można po prostu przekonwertować je oba na małe litery i porównać:

```elm
(String.toLower "Kot") == (String.toLower "KOT") --> True
```

## Głębsze zagadnienia

Funkcja `String.toLower` działa poprawnie dla większości języków, jednak w niektórych przypadkach może być potrzebna bardziej zaawansowana obróbka stringów, na przykład usuwanie akcentów czy usuwanie spacji i innych znaków specjalnych. W takich sytuacjach warto wykorzystać bibliotekę `String.Extra`, która oferuje bardziej zaawansowane funkcje do obsługi stringów.

## Zobacz także

- Dokumentacja funkcji `String.toLower`: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Biblioteka `String.Extra`: https://package.elm-lang.org/packages/elm-community/string-extra/latest/