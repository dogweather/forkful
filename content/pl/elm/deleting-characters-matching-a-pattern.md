---
title:    "Elm: Usuwanie znaków pasujących do wzorca"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego
Często zdarza się, że w trakcie programowania musimy usuwać znaki z danego ciągu według określonego wzoru. W tym artykule dowiesz się, jak można to zrobić w języku programowania Elm.

## Jak to zrobić
Aby usunąć znaki pasujące do określonego wzoru, można użyć funkcji `String.filter` w Elm. Przyjmie ona dwa argumenty - funkcję `pred` (wyrażenie lambda), która będzie testować, czy dany znak pasuje do wzoru, oraz ciąg znaków, z którego chcemy usunąć niepożądane znaki.

```Elm
myString = "123 Elm Programming"
filteredString = String.filter (\c -> c >= "0" && c <= "9") myString -- zwróci "123"
```

W powyższym przykładzie, wykorzystując wyrażenie lambda, tworzymy funkcję `pred`, która sprawdza, czy dany znak jest cyfrą za pomocą warunku `(c >= "0" && c <= "9")`. Następnie, przekazując tę funkcję i oryginalny ciąg znaków do funkcji `String.filter`, uzyskujemy wynikowy ciąg z samymi cyframi.

## Głębszy zanurzenie
Powyższa metoda jest prosta i wygodna, ale nie jest idealna w przypadku bardziej złożonych wzorców. Jeśli chcemy na przykład usunąć wszystkie spacje i znaki przestankowe z ciągu, możemy użyć funkcji `String.toUpper` w połączeniu z funkcją `String.toList`:

```Elm
myString = "To be, or not to be"
filteredString = myString |> String.toList |> List.filter (\c -> c /= " " && (c < "a" || c > "z")) |> String.fromList
-- zwróci "TOBEORNOTTOBE"
```

Wykorzystując funkcję |> (znana także jako pipe operator) możemy przekazać kolejne funkcje do wynikowego ciągu. W powyższym przykładzie, spacje i znaki przestankowe zostają usunięte, a wszystkie litery są zamienione na duże.

## Zobacz także
- Dokumentacja funkcji `String.filter` w Elm: https://package.elm-lang.org/packages/elm/core/1.0.5/String#filter
- Przykłady użycia i wyjaśnienia operatora |> w języku Elm: https://guide.elm-lang.org/appendix/operator_precedence.html# |>