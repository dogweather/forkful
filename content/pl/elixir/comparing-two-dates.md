---
title:                "Elixir: Porównywanie dwóch dat"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być bardzo przydatne w programowaniu, ponieważ pozwala na wykrycie różnic w czasie i podejmowanie odpowiednich działań w zależności od wyniku porównania. Na przykład, jeśli chcemy sprawdzić, czy dana data jest wcześniejsza lub późniejsza niż inna data, możemy użyć porównania dat.

## Jak to zrobić

Możemy użyć modułu Elixir `Date` do porównania dwóch dat. Przykładowo, chcemy porównać datę urodzenia z dzisiejszą datą i sprawdzić, czy osoba jest już pełnoletnia. Oto przykład kodu:

```elixir
today = Date.utc_today()
birth_date = Date.new(1990, 10, 10)

if birth_date > today do
  IO.puts("Ta osoba nie jest jeszcze pełnoletnia")
elsif birth_date < today do
  IO.puts("Ta osoba jest już pełnoletnia")
else
  IO.puts("Ta osoba ma dzisiaj urodziny!")
end
```

Powyższy kod używa funkcji `>`, `>=`, `<`, `<=`, aby porównać dwie daty. Pamiętaj, aby użyć funkcji `utc_today()` do uzyskania dzisiejszej daty w strefie czasowej UTC.

Możemy również użyć biblioteki `Timex` do bardziej zaawansowanych operacji na datach, takich jak porównywanie tylko części dat, np. miesiąca lub dnia. Przykładowo:

```elixir
today = Timex.today()
next_month = Timex.shift(today, months: 1)

if Timex.date_part(today, :month) > Timex.date_part(next_month, :month) do
  IO.puts("Jeszcze nie minął miesiąc")
else
  IO.puts("Minął już miesiąc")
end
```

## Głębszy zanurzenie

Elixir i biblioteka Timex oferują wiele funkcji do porównywania dat. Możemy na przykład użyć funkcji `is_same?(datetime1, datetime2, time_unit)`, aby sprawdzić, czy dwie daty są takie same w wybranej jednostce czasu. Dostępne jednostki to m.in. rok, miesiąc, dzień, godzina, minuta, sekunda. Więcej informacji znajdziesz w dokumentacji modułu `Timex`.

Możemy również porównywać daty w strefach czasowych, korzystając z funkcji `shift_zone(datetime, zone)`, która przesuwa datę do wybranej strefy czasowej.

## Zobacz również

- [Dokumentacja modułu Date w Elixirze](https://hexdocs.pm/elixir/Date.html)
- [Dokumentacja biblioteki Timex](https://hexdocs.pm/timex/readme.html)
- [Porównywanie dat w Elixirze - artykuł na blogu Elixir Mastery](https://elixirmastery.com/blog/date-comparison-in-elixir/)