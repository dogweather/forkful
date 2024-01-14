---
title:    "Elm: Usuwanie znaków pasujących do wzoru"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Dlaczego usuwanie znaków pasujących do wzoru jest ważne w programowaniu Elm?

Usuwanie znaków pasujących do wzoru jest ważne, ponieważ pozwala nam na efektywne przetwarzanie i filtrację tekstu w naszym kodzie. Jest to użyteczne w wielu przypadkach, na przykład gdy chcemy usunąć niepożądane znaki lub tylko pozostawić część tekstu.

## Jak to zrobić?

Aby usunąć znaki pasujące do wzoru w Elm, musimy użyć funkcji `String.filter`. Przykładowy kod wyglądałby następująco:

```Elm 
text = "To jest przykładowy tekst do przefiltrowania" 
filteredText = String.filter (\c -> not (c == "i")) text 
```

W tym przykładzie, każdy znak "i" jest usuwany z tekstu, a wynikiem jest "To jest przykładowy tekst do przeflterowana".

Możemy również użyć bardziej złożonych wzorów, przekazując funkcję warunkową do funkcji `filter`. Na przykład, jeśli chcielibyśmy usunąć wszystkie liczby z tekstu, możemy użyć następującego kodu:

```Elm
text = "Tekst z liczbami: 1, 2, 3, 4" 
filteredText = String.filter (\c -> not (String.isDigit c)) text
```

W rezultacie, otrzymamy "Tekst z liczbami:".

## Głębszy wgląd

Funkcja `String.filter` jest jedną z wielu przydatnych funkcji w modułach `String` w Elm. Inne przydatne funkcje do przetwarzania tekstu to `String.map` i `String.split`. Warto także wspomnieć, że te funkcje działają na zasadzie niezmienialności - zwracają kopię zmienionego tekstu, a nie zmieniają oryginalnego tekstu.

Ponadto, warto pamiętać o wydajności przy używaniu funkcji `String.filter`, ponieważ każda iteracja przez znaki jest wykonywana osobno.

## Zobacz także

- Dokumentacja Elm o [modułach tekstowych](https://package.elm-lang.org/packages/elm/core/latest/String)
- Przekształcaj i filtrowaj tekst z [Elm Exercise](https://elm-exercises.github.io/transform-string/)