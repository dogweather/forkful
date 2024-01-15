---
title:                "Obliczanie daty w przyszłości lub przeszłości."
html_title:           "Ruby: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości jest przydatne, gdy chcemy prognozować terminy, ustalać plany lub wyświetlać dynamiczne informacje na stronie internetowej.

## Jak to zrobić

```ruby
# aby obliczyć datę w przyszłości, dodajemy odpowiednią ilość dni do bieżącej daty:
future_date = Date.today + 30 
# wyświetli 30 dni od dzisiaj
puts "Za miesiąc będzie #{future_date}"

# aby obliczyć datę w przeszłości, odejmujemy odpowiednią ilość dni od bieżącej daty:
past_date = Date.today - 7 
# wyświetli tydzień temu
puts "Tydzień temu było #{past_date}"

# możemy także użyć metod like `next_day`, `prev_day`, `next_month`, `prev_month`, itp.:
next_week = Date.today.next_day(7)
puts "W przyszłym tygodniu będzie #{next_week}"

# jeśli chcemy dodać lub odjąć inne jednostki czasu, np. tygodnie, miesiące, lata, możemy użyć metody `advance` wraz z odpowiednim haszem jako argumentem:
future_date = Date.today.advance(years: 1, months: 2, weeks: 3)
puts "Za rok, 2 miesiące i 3 tygodnie będzie #{future_date}"
```

## Głębsze zanurzenie

Ruby posiada wiele przydatnych metod do manipulowania datami i czasem. Podczas obliczania daty w przyszłości lub przeszłości, możemy wykorzystać te metody, aby uzyskać dokładniejsze wyniki i uniknąć błędów związanych z różnicami w długości miesięcy czy lat.

Możemy także zmienić strefę czasową używaną przez naszą aplikację za pomocą `Time.zone=` lub `Time.zone_local=`. Więcej informacji na ten temat znajdziesz w dokuentacji Ruby.

## Zobacz także

- [Dokumentacja Ruby o dacie i czasie](https://ruby-doc.org/core-3.0.0/Date.html)
- [Atak jak obliczyć datę w Ruby](https://youtu.be/QWdsXYBI6ro)
- [Przydatne metody do manipulowania datami w Ruby](https://medium.com/@acorred1/date-manipulation-using-ruby-a-case-study-7f8d9c13d0eb)