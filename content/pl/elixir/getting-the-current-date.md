---
title:                "Elixir: Pobieranie aktualnej daty"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, w których programiści muszą pobrać aktualną datę w swoim kodzie. Może to być potrzebne do zapisania daty utworzenia pliku, utworzenia nowego wpisu w dzienniku lub wyświetlenia ostatniej aktualizacji na stronie. Wykorzystanie bieżącej daty jest również przydatne podczas tworzenia testów i debugowania.

## Jak to zrobić

W Elixirze istnieją dwa podstawowe sposoby na pobranie bieżącej daty. Pierwszym jest użycie wbudowanej funkcji `Date.utc_today/0`, która zwraca bieżącą datę w formacie UTC. Przykładowy kod wyglądałby tak:

```Elixir
today = Date.utc_today()
IO.puts("Bieżąca data to: #{today}")
```

To wyświetli aktualną datę w formacie: "YYYY-M-D". Jeśli chcemy wyświetlić datę w innym formacie, możemy użyć funkcji `Date.to_string/2`. Na przykład, jeśli chcemy wyświetlić datę w formacie "D.MonthName.YYYY", możemy zmodyfikować nasz kod w następujący sposób:

```Elixir
formatted_date = Date.to_string(today, "D.~w.YYYY")
IO.puts("Bieżąca data to: #{formatted_date}")
```

To wyświetli datę w formacie "D.Miesiąc.Rok", np. "12.Listopad.2021".

Drugim sposobem na pobranie bieżącej daty jest użycie modułu `Calendar`. Możemy użyć funkcji `Calendar.local_time/0`, która zwraca aktualny czas w podanej strefie czasowej. Możemy wybrać strefę czasową, dostosowując argumenty funkcji `local_time`. Przykładowy kod wyglądałby tak:

```Elixir
now = Calendar.local_time({{"Europe", "Warsaw"}, "Etc/UTC"})
IO.puts("Aktualny czas w Warszawie to: #{now}")
```

To wyświetli aktualny czas w strefie czasowej "Europe/Warsaw", w formacie "YYYY-M-D h:m:s".

## Głębsze zagłębienie

W języku Elixir nie ma wbudowanego typu danych dla daty i czasu. Zamiast tego, używa się funkcji z modułów `Date` i `Calendar` do manipulowania datami i czasami. Warto pamiętać, że wszystkie operacje na dacie są niezmiennicze - oznacza to, że funkcje te nie zmieniają oryginalnej daty, ale zwracają nowe wartości.

Można również używać formatów daty i czasu, takich jak ISO 8601, dzięki modułowi `DateTime`. Ten moduł oferuje funkcje do konwertowania daty i czasu na stringi, parsowania stringów na daty i czas i wiele innych. Warto zapoznać się z dokumentacją tego modułu, aby dowiedzieć się więcej.

## Zobacz również

- [Dokumentacja modułu Date w Elixirze](https://hexdocs.pm/elixir/Date.html)
- [Dokumentacja modułu Calendar w Elixirze](https://hexdocs.pm/elixir/Calendar.html)
- [Dokumentacja modułu DateTime w Elixirze](https://hexdocs.pm/elixir/DateTime.html)