---
title:    "Go: Generowanie losowych liczb"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Dlaczego warto generować losowe liczby?

Generowanie losowych liczb jest nieodłączną częścią programowania, niezależnie od wybranej technologii czy języka. W przypadku Go, jest to szczególnie ważne ze względu na jego wszechstronność i wykorzystywanie do różnych celów. W tym artykule dowiecie się, jak generować losowe liczby w Go i dlaczego jest to przydatne.

## Jak to zrobić?

W Go do generowania losowych liczb wykorzystujemy pakiet `math/rand`. Aby uzyskać dostęp do tej funkcjonalności, musimy go zaimportować:

```
import "math/rand"
```

Następnie możemy wykorzystać funkcję `Intn()` do wygenerowania losowej liczby całkowitej mniejszej od zadanej liczby. Na przykład, aby wygenerować liczbę od 1 do 10, wykonujemy:

```
rand.Intn(10) + 1
```

Możemy również wygenerować losową liczbę zmiennoprzecinkową w zakresie od 0 do 1 za pomocą funkcji `Float64()`:

```
rand.Float64()
```

Warto również wspomnieć o funkcji `Seed()`, która inicjuje generator liczb losowych. Jeśli nie podamy wartości do tej funkcji, generator będzie korzystał z wartości domyślnej, przez co wyniki będą takie same przy każdym uruchomieniu programu. Aby uzyskać różne wyniki przy każdym uruchomieniu, możemy wykorzystać na przykład bieżący czas jako wartość seed:

```
rand.Seed(time.Now().UnixNano())
```

## Głębsze zagadnienia

Generowanie losowych liczb w Go może wydawać się proste, jednak warto zwrócić uwagę na kilka aspektów. Po pierwsze, korzystając z funkcji `Intn()`, nie uzyskujemy dokładnie równomiernych liczb w zakresie, ponieważ jest to metoda znana jako generacja pseudolosowa. Oznacza to, że liczby są wygenerowane w nieregularny sposób, ale w sposób zaprogramowany, co w niektórych przypadkach może prowadzić do pewnych wzorców i niepełnej losowości.

Kolejnym aspektem wartym uwagi jest wydajność. Generowanie losowych liczb jest operacją dość kosztowną, szczególnie gdy musimy wygenerować dużą ilość liczb. W takim przypadku warto zastanowić się nad wykorzystaniem dodatkowych bibliotek lub algorytmów, które zapewnią lepszą wydajność.

## Zobacz również

- Dokumentacja pakietu `math/rand` w oficjalnej dokumentacji Go: https://golang.org/pkg/math/rand/
- Przykładowe wykorzystanie generowania losowych liczb w grze w konsoli napisanej w Go: https://github.com/rayhannabi/go-guessing-game
- Wprowadzenie do podstaw programowania w Go, w tym generowania losowych liczb: https://blog.golang.org/golang-nuts-and-bolts

Dziękujemy za lekturę i zachęcamy do dalszego poznawania języka Go!