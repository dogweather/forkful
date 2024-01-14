---
title:    "Elm: Porównanie dwóch dat"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego 

Często przy tworzeniu aplikacji internetowych lub mobilnych, musimy porównywać różne daty między sobą. Niezależnie od tego czy są to daty ważnych wydarzeń, daty ważności czy też daty urodzenia, niezawodnie musimy umieć porównywać je ze sobą. Wstęp do porównywania dat w języku Elm jest niezbędny dla każdego programisty, który tworzy aplikacje z wykorzystaniem tego języka.

## Jak to zrobić 

Aby porównywać daty w języku Elm, musimy skorzystać z modułu `Time`. Ten moduł został stworzony specjalnie do obsługi formatowania i operacji na datach. Poniżej przedstawione są przykładowe kody, które pokazują jak porównywać daty:

```Elm
-- Zaimportowanie modułu `Time` jest wymagane

import Time exposing (..)

-- Porównanie dwóch dat

compareDates : Time.Posix -> Time.Posix -> Order
compareDates date1 date2 =
  if date1 > date2 then
    GT
  else if date1 < date2 then
    LT
  else
    EQ

-- Formatowanie daty

formatDate : Time.Posix -> String
formatDate date =
  format "%d/%m/%Y" date
```

Użycie funkcji `compareDates` pozwala na porównywanie dwóch dat, a jej wynikiem jest wartość `Order`, która wskazuje, która data jest większa. Natomiast funkcja `formatDate` pozwala na sformatowanie daty w wybranym przez nas formacie.

## Głębszy wgląd 

Podczas porównywania dat, należy pamiętać o tym, że odpowiednie formatowanie jest kluczowe. W języku Elm, daty są przechowywane w formacie `Time.Posix`, ale można je również przekształcić do innych formatów, takich jak `Time.Year`, `Time.Month`, `Time.Day`, itp. W przypadku porównywania dat nie liczy się jedynie porównanie wartości, ale również odpowiednie formatowanie. Warto również pamiętać, że operacje na dużych liczbach mogą być czasochłonne, dlatego dobrze jest upewnić się, że porównujemy odpowiednie daty przed przystąpieniem do samych operacji porównania.

## Zobacz również 

Jeśli chcesz dowiedzieć się więcej o porównywaniu dat w języku Elm, polecamy zapoznać się z dokumentacją modułu `Time` oraz przeglądnąć poniższe linki:

- [Dokumentacja modułu Time w języku Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Strona główna języka Elm](https://elm-lang.org/)
- [Porównywanie dat w języku Elm - poradnik programisty](https://medium.com/coding-secrets/comparing-dates-in-elm-eba1d9e3a6ed)