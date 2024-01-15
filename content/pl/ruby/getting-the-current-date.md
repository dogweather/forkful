---
title:                "Pobieranie aktualnej daty"
html_title:           "Ruby: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie z użyciem Ruby jest nie tylko przyjemne, ale także może być bardzo przydatne w codziennej pracy z różnymi projektami. Jednym z często wykonywanych zadań jest pobieranie aktualnej daty i godziny w celu wykonywania różnych operacji. W tym artykule dowiesz się, jak w łatwy sposób uzyskać aktualną datę w języku Ruby.

## Jak to zrobić

Aby uzyskać aktualną datę w języku Ruby, wystarczy wywołać metodę `Time.now`, która zwróci bieżący czas w formacie `YYYY-MM-DD HH:MM:SS +HHMM`. Dla przykładu, jeśli uruchomisz poniższy kod:

```Ruby
puts Time.now
```

Otrzymasz wynik:

```Ruby
2021-06-14 15:30:00 +0200
```

Możesz również użyć metody `strftime`, aby sformatować datę według własnych preferencji. Na przykład, jeśli chcesz wyświetlić tylko dzień miesiąca i nazwę miesiąca, możesz użyć poniższego kodu:

```Ruby
puts Time.now.strftime("%d-%B")
```

Otrzymasz wynik:

```Ruby
14-czerwiec
```

Inne przydatne formaty, które możesz użyć z metodą `strftime`, to np. `%a` do wyświetlenia dnia tygodnia w skrócie lub `%H:%M` do wyświetlenia tylko godziny i minut.

## Deep Dive

W Ruby bieżący czas jest przechowywany jako obiekt `Time`, który zawiera informacje o roku, miesiącu, dniu, godzinie, minucie i sekundzie. Możesz wywołać metody takie jak `year`, `month`, `day` itp., aby uzyskać te informacje. Przykładowo:

```Ruby
today = Time.now
puts today
puts today.year
puts today.month
puts today.day
```

Otrzymasz wynik:

```Ruby
2021-06-14 15:30:00 +0200
2021
6
14
```

Możesz również wykonywać różne operacje na datach, na przykład dodając lub odejmując dni, godziny lub minuty. Wystarczy użyć metody `+` lub `-` i podać liczbę, którą chcesz dodać lub odjąć. Przykładowo:

```Ruby
tomorrow = Time.now + 86400 # dodaje 24 godziny
yesterday = Time.now - 86400 # odejmuje 24 godziny
next_hour = Time.now + 3600 # dodaje 1 godzinę
```

## Zobacz również

- [Dokumentacja Ruby o obiektach Time](https://ruby-doc.org/core-3.0.0/Time.html)
- [Inne sposoby uzyskiwania aktualnej daty w Ruby](https://www.rubyguides.com/ruby-tutorial/get-date-time/)
- [Poradnik Ruby z przykładami i ćwiczeniami](https://www.ruby-lang.org/pl/documentation/quickstart/)

*Dzięki za przeczytanie! Mam nadzieję, że ten artykuł pomógł Ci w uzyskaniu aktualnej daty w języku Ruby.*