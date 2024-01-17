---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Elixir: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?

Obliczanie daty w przyszłości lub przeszłości jest działaniem, które polega na wykorzystaniu języka programowania do wyliczenia daty, która znajduje się w przyszłości lub przeszłości od aktualnej daty. Programiści często wykonują to, aby automatyzować procesy lub prowadzić obliczenia na danych zawierających informacje o dacie.

## Jak to zrobić?

```Elixir
# Obliczanie dzisiejszej daty
Date.utc_today()

# Dodanie 3 dni do dzisiejszej daty
Date.utc_today() |> Date.add(3)

# Odejmowanie 5 dni od dzisiejszej daty
Date.utc_today() |> Date.add(-5)

# Wyświetlenie daty w innym formacie
Date.utc_today() |> Date.to_string("{YYYY}-{0M}-{0D}")
```

Przykładowy wynik:
```
2022-01-01
```

## Pogłębione informacje

1. W przeszłości, obliczanie daty w przyszłości lub przeszłości było bardzo czasochłonnym procesem, ponieważ wymagało manualnego liczenia dni. Dzięki rozwojowi języków programowania, takich jak Elixir, ten proces stał się prostszy i szybszy.
2. Alternatywą dla Elixir może być użycie języków programowania takich jak Java czy Python, które również posiadają funkcje do obliczania daty.
3. Obliczanie daty w przyszłości lub przeszłości wymaga dokładnego zrozumienia kalendarza gregoriańskiego oraz sposobu jego liczenia dni.

## Zobacz również

Linki do źródeł związanych z obliczaniem daty w przyszłości lub przeszłości:
- Dokumentacja Elixir: https://hexdocs.pm/elixir/master/Date.html
- Przykładowy kod w języku Python: https://www.programiz.com/python-programming/datetime/strptime
- Funkcja w języku Java: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html