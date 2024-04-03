---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:01.392250-07:00
description: 'Hoe te: In Elixir gebruiken we vaak pattern matching en de `case` statement
  om verschillende uitkomsten, inclusief fouten, te behandelen.'
lastmod: '2024-03-13T22:44:50.469576-06:00'
model: gpt-4-0125-preview
summary: In Elixir gebruiken we vaak pattern matching en de `case` statement om verschillende
  uitkomsten, inclusief fouten, te behandelen.
title: Fouten afhandelen
weight: 16
---

## Hoe te:
In Elixir gebruiken we vaak pattern matching en de `case` statement om verschillende uitkomsten, inclusief fouten, te behandelen.

```elixir
defmodule Voorbeeld do
  def delen(a, b) do
    case b do
      0 -> {:error, "Kan niet delen door nul."}
      _ -> {:ok, a / b}
    end
  end
end

# Succesvolle deling
{:ok, resultaat} = Voorbeeld.delen(10, 2)
IO.puts("10 / 2 is #{resultaat}")

# Poging tot delen door nul
{:error, reden} = Voorbeeld.delen(10, 0)
IO.puts("Fout: #{reden}")
```

Voorbeeld van uitvoer:
```
10 / 2 is 5.0
Fout: Kan niet delen door nul.
```

Wanneer je deze Elixir-code uitvoert, krijg je ofwel een succesvolle deling of een foutmelding, afhankelijk van je invoer. Geen crashes hier!

## Diepgaande duik
Vroeger was foutafhandeling vaak gebaseerd op het controleren van retourwaarden. Met de functionele roots van Elixir hebben we echter pattern matching en getagde tuples, zoals `{:ok, waarde}` of `{:error, reden}`, die eleganter zijn.

Er zijn andere manieren om fouten in Elixir te behandelen:

- **Elixir's `try` en `rescue`** die lijken op de traditionele `try-catch` in imperatieve talen, maar worden minder vaak gebruikt vanwege de voorkeur van Elixir voor explicietheid.
- **Supervisors en GenServers**, onderdeel van Elixir's OTP-framework, die meer over fouttolerantie gaan. Ze houden het proces van je code in de gaten, klaar om het te herstarten als er iets misgaat.

Wat de implementatie betreft, bouwt Elixir voort op de robuustheid van Erlang. Het behandelt fouten als gewoon een ander type bericht dat moet worden afgehandeld met alle pattern matching en functionele goedheid.

## Zie ook
Voor verder lezen over foutafhandeling in Elixir, bekijk:

- De officiële gids van Elixir over [foutafhandeling](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Leer meer over [processen en OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Het Elixir Forum is altijd een goede plek om vragen te stellen: [https://elixirforum.com](https://elixirforum.com).
