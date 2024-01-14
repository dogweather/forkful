---
title:                "Ruby: Porównywanie dwóch dat"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy porównywać dwie daty. Może to być konieczne, aby upewnić się, czy dwa wydarzenia wystąpiły w różnych dniach lub aby wyświetlić tylko aktualne lub przeszłe wpisy. W tym artykule opowiem Ci, jak porównywać dwie daty w języku Ruby.

## Jak to zrobić

Porównywanie dat w Ruby jest bardzo proste. Musimy tylko użyć odpowiednich metod, aby otrzymać pożądany wynik.

```Ruby
d1 = Date.new(2021, 1, 1) # pierwsza data
d2 = Date.new(2020, 12, 31) # druga data

puts d1 > d2 # false
puts d1 < d2 # true
puts d1 == d2 # false
```

W powyższym przykładzie tworzymy dwa obiekty daty i używamy operatorów porównania, aby sprawdzić relację między nimi. Możemy również użyć metod `after?` i `before?` do porównania dat.

```Ruby
d1 = Date.new(2021, 1, 1) # pierwsza data
d2 = Date.new(2020, 12, 31) # druga data

puts d1.after?(d2) # true
puts d2.before?(d1) # true
```

Warto również zwrócić uwagę, że możemy porównywać daty zawarte w obiektach różnych klas, takich jak `DateTime`, `Time` i `Date`.

## Głębszy zanurzenie

Metody `after?` i `before?` porównują daty ze sobą, ale co jeśli chcemy sprawdzić, czy dana data znajduje się między dwiema innymi datami? Do tego celu możemy użyć metody `between?`.

```Ruby
d1 = Date.new(2021, 1, 1) # data początkowa
d2 = Date.new(2021, 2, 1) # data końcowa
check_date = Date.new(2021, 1, 15) # data do sprawdzenia

puts check_date.between?(d1, d2) # true
```

Możemy również porównywać daty z wykorzystaniem operatorów logicznych `&&` i `||`. Na przykład, jeśli chcemy sprawdzić, czy data znajduje się pomiędzy dwoma innymi datami, możemy napisać:

```Ruby
d1 = Date.new(2021, 1, 1) # data początkowa
d2 = Date.new(2021, 2, 1) # data końcowa
check_date = Date.new(2021, 1, 15) # data do sprawdzenia

puts check_date > d1 && check_date < d2 # true
```

## Zobacz także

- [Dokumentacja Ruby o porównywaniu dat](https://ruby-doc.org/core-3.0.0/Comparable.html#method-i-between-3F)
- [Porównywanie dat w języku Ruby - tutorial na YouTube (po angielsku)](https://www.youtube.com/watch?v=6_qI_L_68wc)