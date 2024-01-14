---
title:    "Elixir: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Dlaczego

Czasami potrzebujemy wyliczyć datę w przyszłości lub przeszłości w naszych aplikacjach Elixir. Może to być na przykład w celu wyświetlenia przyszłych lub poprzednich wydarzeń kalendarzowych, planowania zadań lub ustawiania przypomnień. W tym artykule omówimy sposób wyliczania dat w przód lub w tył za pomocą Elixir.

# Jak to zrobić

Przykładowy kod:

```Elixir
date = Date.utc_today()
in_5_days = Date.add(date, 5, :days)
IO.puts in_5_days
```

W powyższym przykładzie użyliśmy funkcji `Date.add/3`, która dodaje określoną liczbę dni do aktualnej daty. Możemy również użyć innych jednostek, takich jak sekundy, minuty, godziny, tygodnie, miesiące czy lata. Funkcja ta zwraca nowy obiekt daty, więc musimy go wyswietlić za pomocą `IO.puts` lub przechowywać w zmiennej.

Kod w poniższym bloku pomaga nam wyliczyć datę w przeszłości:

```Elixir
birthday = Date.from_iso8601("1990-03-15")
years_ago = Date.add(birthday, -30, :years)
IO.puts years_ago
```

W ten sposób możemy wyliczyć datę 30 lat wstecz od daty urodzin, której użyliśmy do zmienniej `birthday`.

# Głębszy zanurzenie

Funkcja `Date.add/3` wykorzystuje kalendarz Gregoriański do obliczenia nowej daty, co może prowadzić do nieoczekiwanych wyników w przypadku dat przed wprowadzeniem tego kalendarza w roku 1582. W takich przypadkach lepiej jest skorzystać z funkcji `Calendar.DateTime.add/4` lub `Calendar.Date.add/3`, które pozwalają na wybieranie innego kalendarza, takiego jak Julian czy Islaamic.

Ponadto, jeśli potrzebujemy wyliczyć datę na podstawie innej jednostki czasu, na przykład dodając 36 godzin do obecnej daty, użyjemy funkcji `NaiveDateTime.add/3` lub `DateTime.add/3`. Pierwsza z nich obsługuje jedynie strefy czasowe UTC, podczas gdy druga bierze pod uwagę strefy czasowe i przeprowadza konwersję wewnętrznie.

# Zobacz też

- Dokumentacja Elixir dla funkcji `Date.add/3` (https://hexdocs.pm/elixir/Date.html#add/3)
- Dokumentacja Elixir dla modułu `Calendar` (https://hexdocs.pm/elixir/Calendar.html)
- Wprowadzenie do dat i czasu w Elixir (https://michal.muskala.eu/2017/02/16/date-and-time.html)