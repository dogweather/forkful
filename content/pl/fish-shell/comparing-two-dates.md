---
title:    "Fish Shell: Porównywanie dwóch dat."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być niezbędnym zadaniem w wielu projektach programistycznych. Niezależnie od tego, czy chcesz sprawdzić, czy dana data jest wcześniejsza czy późniejsza, czy też potrzebujesz wykonać określone działania na dwóch różnych datach, znajomość techniki porównywania dat w języku Fish Shell jest niezwykle przydatna.

## Jak to zrobić

Aby porównać dwie daty w języku Fish Shell, możesz użyć wbudowanego polecenia ```date```. Poniżej znajdują się kilka przykładów tego, jak możesz wykorzystać to polecenie:

```
fish
set data1 "2021-01-01"
set data2 "2021-01-15"
if test (date -f "%Y-%m-%d" $data1) -ge (date -f "%Y-%m-%d" $data2)
    echo $data1 jest niepóźniejsza lub równa $data2
else
    echo $data1 jest wcześniejsza niż $data2
end
```

W powyższym przykładzie użyliśmy polecenia test, aby porównać dwie daty zapisane jako zmienne. Użyliśmy również opcji ```-ge``` (greater than or equal), aby sprawdzić, czy pierwsza data jest większa lub równa drugiej. Jeśli tak, wyświetlamy odpowiedni komunikat.

Możesz także wykorzystać polecenie ```date``` do przeprowadzenia innych operacji na dacie, na przykład konwersji jej do innego formatu lub obliczenia różnicy między dwoma datami.

## Głębszy zanurzenie

Aby lepiej zrozumieć działanie porównywania dat w języku Fish Shell, warto poznać jego dokładniejsze mechanizmy. Na przykład, aby dokładnie wyświetlić różnicę między dwiema datami, możesz użyć polecenia ```math```, aby odjąć od siebie dwa wartości czasu uzyskane za pomocą polecenia ```date```.

```
set time1 (date +%s)
set time2 (date +%s)
set difference (math $time2 - $time1)
echo "Różnica między datami w sekundach:" $difference
```

Opcja ```+%s``` w poleceniu ```date``` zwraca czas w sekundach od epoki UNIX, co ułatwia obliczenie różnicy.

## Zobacz także

- Dokumentacja polecenia ```date``` w języku Fish Shell: https://fishshell.com/docs/current/cmds/date.html
- Przewodnik po obsłudze dat w języku Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_dates
- Przykłady użycia polecenia ```date``` w języku Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_date_f