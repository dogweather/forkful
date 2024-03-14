---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:02.198259-07:00
description: "Tests schrijven in programmeren is code maken om te controleren of andere\
  \ code correct werkt. Programmeurs doen dit om vroegtijdig fouten te vinden, te\u2026"
lastmod: '2024-03-13T22:44:50.465691-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven in programmeren is code maken om te controleren of andere\
  \ code correct werkt. Programmeurs doen dit om vroegtijdig fouten te vinden, te\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven in programmeren is code maken om te controleren of andere code correct werkt. Programmeurs doen dit om vroegtijdig fouten te vinden, te bevestigen dat nieuwe functies de oude niet breken, en beter te slapen wetende dat hun code solide is.

## Hoe:

In Elixir gebruik je ExUnit om tests te schrijven. Het is een ingebouwd framework dat vriendelijk en eenvoudig te beginnen is. Hier is een snel voorbeeld:

```elixir
# test/voorbeeld_test.exs
defmodule VoorbeeldTest do
  use ExUnit.Case

  test "de waarheid" do
    assert 1 + 1 == 2
  end
end
```

Draai het met `mix test`:

```shell
$ mix test
..

Klaar in 0,03 seconden
1 test, 0 fouten
```

Goed! Je hebt een test geschreven die bevestigt dat wiskunde niet is veranderd.

## Diepe Duik

Tests zijn een groot punt in Elixir sinds José Valim de taal leven inblies, geïnspireerd door Ruby's testcultuur. Alternatieven? Niet veel binnen de wereld van Elixir – ExUnit is de go-to. Je zou echter eigenschap-gebaseerd testen met StreamData kunnen verkennen of duiken in het mocken met Mox voor meer complexe scenario's. Tests gaan helemaal over het bevestigen van verwachte uitkomsten—wat je hebt gezien met `assert`—maar er is ook `refute` voor het specificeren van wat niet zou moeten gebeuren.

## Zie Ook

Om je vaardigheden in het schrijven van tests te verbeteren, bekijk deze:

- Elixir's testgidsen: https://hexdocs.pm/ex_unit/ExUnit.html
- StreamData voor eigenschap-gebaseerd testen: https://hexdocs.pm/stream_data/StreamData.html
- Mocken met Mox: https://hexdocs.pm/mox/Mox.html

Ga nu wat code test-driven!
