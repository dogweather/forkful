---
title:                "Porównywanie dwóch dat"
html_title:           "C: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat w C jest niezbędne, ponieważ czas i daty są powszechnie używane w programowaniu i mogą wpływać na zachowanie programu. Porównanie dat pozwala nam na kontrolę przebiegu wydarzeń i podejmowanie odpowiednich działań w zależności od wyniku porównania.

## Jak to zrobić

Aby porównać dwie daty w C, należy najpierw zadeklarować zmienne typu `struct tm`, które przechowują informacje o dacie i czasie. Następnie należy ustawić wartości tych zmiennych za pomocą funkcji `gmtime()` lub `localtime()` w zależności od tego, czy chcemy porównać daty w strefie czasowej lokalnej czy w strefie czasowej GMT.

Po ustawieniu wartości można skorzystać z funkcji `difftime()` do porównania dwóch dat. Funkcja ta zwraca różnicę czasu w sekundach między dwoma podanymi datami. Aby uzyskać wynik w innych jednostkach, można użyć odpowiednich funkcji takich jak `difftimesec()`, `difftimemin()` lub `difftimehour()`.

Zobacz poniższy przykład kodu:

```C
// Deklarowanie zmiennych przechowujących daty
struct tm date1, date2;

// Ustawianie wartości zmiennych
date1 = *gmtime(...)
date2 = *localtime(...)

// Porównywanie dat i przechowywanie wyniku w zmiennej typu double
double result = difftime(mktime(&date1), mktime(&date2));

// Wyświetlanie wyniku w sekundach
printf("Różnica czasu między dwoma datami wynosi: %f sekund\n", result);
```

W powyższym przykładzie `gmtime()` i `localtime()` przyjmują jako argumenty wartości daty i czasu, natomiast `mktime()` konwertuje podane daty i czasy na format, który jest możliwy do porównania przez `difftime()`.

## Głębszy wybór

Istnieje wiele funkcji w C, które umożliwiają porównywanie dat i czasu, takich jak `difftime()`, `difftime64()` czy `difftime64i32()`. Większość z nich działa w podobny sposób, jednak różni się dokładnością czasu oraz możliwościami pracy z datami przed i po roku 2038.

Podczas porównywania dat warto zwrócić uwagę na różnice czasowe, takie jak letnie i zimowe czasu, które mogą wpłynąć na wynik. Warto również pamiętać o ustawieniu prawidłowej strefy czasowej podczas deklarowania zmiennych typu `struct tm`.

## Zobacz również

- [Dokumentacja C dla funkcji `difftime()`] (https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm)
- [Przydatne kody C dla porównywania dat] (https://www.geeksforgeeks.org/comparing-two-dates-in-c/)
- [Tutorial o pracy z datami i czasem w C] (https://www.guru99.com/c-date-time.html)