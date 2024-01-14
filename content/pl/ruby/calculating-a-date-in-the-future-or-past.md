---
title:    "Ruby: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być niezbędne w wielu sytuacjach programistycznych, takich jak tworzenie harmonogramów lub przetwarzanie danych historycznych. W tym artykule dowiesz się, jak w prosty sposób osiągnąć ten cel w Ruby.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w języku Ruby, można użyć metody `DateTime#advace` lub `DateTime#-`, aby dodać lub odjąć odpowiednią ilość dni, tygodni, miesięcy lub lat do istniejącej daty. 

```Ruby
require 'date'

current_date = DateTime.now
# 2021-05-31T17:32:58+02:00

future_date = current_date.advance(days: 5, months: 2)
# 2021-08-05T17:32:58+02:00

past_date = current_date - 10
# 2021-05-21T17:32:58+02:00
```

W powyższych przykładach wykorzystano metodę `advance`, aby dodać 5 dni i 2 miesiące do bieżącej daty, oraz metodę `-`, aby odjąć 10 dni.

Można także użyć metody `DateTime#parse` w połączeniu z metodami `advance` lub `-` do obliczania daty na podstawie tekstu. Na przykład:

```Ruby
future_date = DateTime.parse("July 15, 2021").advance(months: 3)
# 2021-10-15T00:00:00+02:00
```

Pamiętaj, że metody `DateTime#advance` i `DateTime#-` zwracają nowy obiekt daty, a nie modyfikują oryginalnej daty. Dlatego jeśli chcesz zachować oryginalną datę, musisz przypisać nowy obiekt do zmiennej.

## Głębszy przegląd

Obie metody `DateTime#-` i `DateTime#advance` są częścią klasy `DateTime`, która jest częścią biblioteki standardowej języka Ruby. Oznacza to, że można ich używać bez konieczności importowania dodatkowych bibliotek.

W przypadku metody `advance` parametrami są liczbowe nazwy jednostek czasu, takie jak `days`, `weeks`, `months`, `years`. Zwraca ona obiekt daty, który jest odpowiednio przesunięty.

Metoda `DateTime#-` jest zdefiniowana jako odbicie lustrzane metody `DateTime#advance`. Jego parametrami są liczby reprezentujące okres czasu, który ma zostać odjęty od daty.

## Zobacz także

- [Ruby - Dokumentacja metody DateTime#advance](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html#method-i-advance)
- [Ruby - Dokumentacja metody DateTime#-](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html#method-i-23)
- [Ruby - Dokumentacja klasy DateTime](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)