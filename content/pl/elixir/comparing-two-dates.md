---
title:                "Elixir: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego
Porównywanie dwóch dat jest nieodłączną częścią wielu programów, w tym również w Elixir. Dzięki temu możemy dokonywać różnych operacji związanych z datami, takich jak sprawdzanie czy jedna data występuje przed lub po drugiej, obliczanie różnicy czasu między nimi czy też sortowanie danych chronologicznie. Jest to jednocześnie bardzo przydatne narzędzie w pracy z danymi, które są zapisane w formie dat.

## Jak to zrobić
Porównywanie dwóch dat w Elixir jest bardzo proste i nie wymaga dużego nakładu pracy. Wystarczy użyć funkcji `Date.compare/2`, która przyjmuje dwa parametry - dwie daty do porównania. Jeśli pierwsza data jest wcześniejsza niż druga, funkcja zwróci wartość mniejszą od zera. Jeśli natomiast druga data jest wcześniejsza, to zwróci wartość większą od zera. W przypadku gdy obie daty są takie same, funkcja zwróci zero.

```Elixir
# Przykład 1
# Porównanie dat: 1 stycznia 2021 i 31 grudnia 2020
Date.compare( ~D[2020-12-31], ~D[2021-01-01] )

# Wynik: -1 (pierwsza data jest wcześniejsza)

# Przykład 2
# Porównanie dat: 1 stycznia 2021 i 1 stycznia 2021
Date.compare( ~D[2021-01-01], ~D[2021-01-01] )

# Wynik: 0 (obie daty są takie same)

# Przykład 3
# Porównanie dat: 2 lutego 2021 i 1 stycznia 2021
Date.compare( ~D[2021-02-02], ~D[2021-01-01] )

# Wynik: 1 (druga data jest wcześniejsza)
```

Możemy także wykorzystać funkcję `Date.after?/2` lub `Date.before?/2`, które zwrócą wartość logiczną `true` lub `false` w zależności od wyniku porównania. Uwaga - funkcje te porównują daty z dokładnością do jednego dnia, więc jeśli obie daty są takie same, zwrócą `false`.

```Elixir
# Przykład 1
# Czy data 1 stycznia 2021 jest po dacie 31 grudnia 2020?
Date.after?(~D[2021-01-01], ~D[2020-12-31])

# Wynik: true

# Przykład 2
# Czy data 1 stycznia 2021 jest przed datą 31 grudnia 2020?
Date.before?(~D[2021-01-01], ~D[2020-12-31])

# Wynik: false
```

## Głębszy zanurzenie
Podczas porównywania dat w Elixir należy pamiętać o kilku ważnych aspektach. Po pierwsze, funkcje porównujące uwzględniają nie tylko samą datę, ale także godzinę oraz strefę czasową. Dzięki temu możemy precyzyjnie określić, która data jest wcześniejsza lub późniejsza. Po drugie, dwa razy w roku (w przypadku przestępnych lat) może wystąpić problem z porównywaniem dat, ponieważ jedna z dat będzie miała 366 dni, a druga tylko 365. W takim przypadku należy skorzystać z funkcji `DateTime.compare/2`, której działanie jest bardzo podobne do `Date.compare/2`, ale uwzględnia także informację o przestępności roku.

## Zobacz także
- [Dokumentacja Elixir-Date](https://hexdocs.pm/elixir/Date.html)
- [Dokumentacja Elixir-DateTime](https://hexdocs.pm