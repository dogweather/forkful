---
title:                "Ruby: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być ważne w wielu przypadkach programowania. Na przykład, gdy pracujemy z danymi historycznymi lub chcemy ustalić różnicę czasu między dwoma wydarzeniami. W tym wpisie pokażę Ci, jak porównać dwie daty w języku Ruby.

## Jak to zrobić

Aby porównać dwie daty w Ruby, musimy użyć metody `compare` z klasy `Date`. Składnia tej metody jest następująca:

```Ruby
Date.compare(date1, date2)
```

Następnie otrzymamy wynik w postaci liczby, która będzie nam mówić o relacji pomiędzy tymi dwoma datami. Jeśli wynik będzie dodatni, oznacza to, że `date1` jest większe niż `date2`, jeśli będzie ujemny, to `date2` jest większe, a jeśli wynik będzie równy 0, to obie daty są równe.

Spróbujmy tego na przykładzie. Stwórzmy dwie zmienne z datami:

```Ruby
date1 = Date.new(2021, 3, 12)
date2 = Date.new(2021, 3, 15)
```

Teraz wywołajmy metodę `compare`:

```Ruby
Date.compare(date1, date2)
```

Wynikiem będzie liczba `-1`, ponieważ `date2` jest większe niż `date1`.

## Pogłębione spojrzenie

Jeśli zainteresowały Cię bardziej szczegóły dotyczące porównywania dat w języku Ruby, zapoznaj się z dokumentacją klasy `Date` oraz z metodą `compare`. Pamiętaj, że w Ruby daty można porównywać nie tylko za pomocą `compare`, ale także za pomocą operatorów porównania, takich jak `<`, `>`, `<=` itp.

## Zobacz również

- [Dokumentacja Ruby dla klasy Date](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Przykłady porównywania dat w Ruby](https://www.rubyguides.com/2019/04/date-comparison-in-ruby/)