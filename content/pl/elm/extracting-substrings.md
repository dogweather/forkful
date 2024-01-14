---
title:    "Elm: Wycinanie podciągów"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Ekstrahowanie podciągów jest przydatnym narzędziem podczas programowania w Elm. Pozwala na wyodrębnienie konkretnych fragmentów tekstu, co może być przydatne w wielu różnych zastosowaniach. Dzięki temu narzędziu można być bardziej precyzyjnym i elastycznym w manipulowaniu tekstem.

## Jak To Zrobić

Poniżej przedstawiam przykładowy kod w Elm, który demonstruje wykorzystanie funkcji `String.slice` do ekstrahowania podciągów.

```Elm
tekst = "Witaj w świecie Elm!"

pierwszaLitera = String.slice 0 1 tekst -- zwraca "W"
drugiKluczowyWyraz = String.slice 6 10 tekst -- zwraca "świe"
ostatnieCzteryLitery = String.slice -4 0 tekst -- zwraca "Elm!"
```

Wyżej przedstawiony kod używa funkcji `String.slice`, która przyjmuje trzy argumenty - indeks początkowy, indeks końcowy i tekst, z którego ma zostać wyodrębniony podciąg. Funkcja ta zwraca nowy tekst zawierający wybrany podciąg. Pierwszy argument może przyjmować wartość ujemną, co oznacza liczenie od końca tekstu.

## Głębsze Wprowadzenie

Funkcja `String.slice` jest jedną z wielu przydatnych funkcji do manipulacji tekstami w Elm. Innymi funkcjami są na przykład `String.left` i `String.right`, które wybierają odpowiednio liczbę znaków z lewej i prawej strony tekstu. Możliwe jest również zastosowanie funkcji `String.split` do dzielenia tekstu na podciągi na podstawie określonego separatora.

Istnieje również możliwość wykorzystania ekstrahowania podciągów w celu filtracji tekstu. Na przykład, można użyć funkcji `List.filter` aby wybrać wszystkie wyrazy, które zaczynają się od określonej litery.

## Zobacz Również

- [Dokumentacja Elm o ekstrahowaniu podciągów](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [Przydatne funkcje do manipulacji tekstami w Elm](https://medium.com/@jose_gil/elm-text-manipulation-functions-26519451210e)
- [Przewodnik po Elm](https://guide.elm-lang.org/)