---
title:    "Fish Shell: Porównywanie dwóch dat"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat może być niezbędne podczas pisania skryptów w Fish Shell. Dzięki temu można łatwo porównywać różne wydarzenia lub ustalać, które daty są wcześniejsze lub późniejsze. W tym artykule dowiesz się, jak w prosty sposób porównywać daty w Fish Shell.

## Jak to zrobić

Porównywanie dat w Fish Shell jest bardzo proste. Wystarczy użyć funkcji `test` i operatora `=`. Poniżej znajduje się przykładowy kod, który porównuje dwie daty i wyświetla odpowiednią informację:

```
set start_date (date -f %Y-%m-%d 2021-01-01)
set end_date (date -f %Y-%m-%d 2021-05-01)

if test $start_date = $end_date
    echo "Daty są identyczne"
else if test $start_date > $end_date
    echo "Data początkowa jest późniejsza niż data końcowa"
else
    echo "Data początkowa jest wcześniejsza niż data końcowa"
end
```

Powyższy kod używa komendy `date` do ustawienia dwóch dat: `start_date` na 1 stycznia 2021 i `end_date` na 1 maja 2021. Następnie używa funkcji `test` i operatora `=` do porównania tych dat. W zależności od wyniku, wyświetla odpowiednią informację. W tym przypadku, ponieważ data początkowa jest wcześniejsza niż data końcowa, zostanie wyświetlony komunikat "Data początkowa jest wcześniejsza niż data końcowa".

## Pogłębione badanie

Podczas porównywania dat w Fish Shell, warto zwrócić uwagę na format daty, który jest używany przez funkcję `date`. W powyższym przykładzie użyto formatu `%Y-%m-%d`, który oznacza rok-miesiąc-dzień. Jednak istnieją również inne formaty, np. `%d.%m.%Y` lub `%m/%d/%Y`. Ważne jest, aby używać tego samego formatu daty dla obu dat, w przeciwnym razie wyniki porównania mogą być niepoprawne.

## Zobacz również

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/cmds/test.html
- Przydatne porady dotyczące porównywania dat: https://devhints.io/date-comparison