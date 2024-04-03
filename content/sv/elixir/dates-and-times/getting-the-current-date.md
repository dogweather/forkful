---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:27.081064-07:00
description: "Hur: Elixirs standardbibliotek, genom `DateTime`-modulen, till\xE5ter\
  \ h\xE4mtning av det aktuella datumet och tiden. Eftersom Elixir k\xF6rs p\xE5 Erlang\
  \ VM (BEAM),\u2026"
lastmod: '2024-03-13T22:44:37.577967-06:00'
model: gpt-4-0125-preview
summary: "Elixirs standardbibliotek, genom `DateTime`-modulen, till\xE5ter h\xE4mtning\
  \ av det aktuella datumet och tiden."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur:
Elixirs standardbibliotek, genom `DateTime`-modulen, tillåter hämtning av det aktuella datumet och tiden. Eftersom Elixir körs på Erlang VM (BEAM), utnyttjar det de underliggande Erlang-funktionaliteterna för tidsoperationer.

### Användning av Elixirs Standardbibliotek
Elixir tillhandahåller funktionen `DateTime.utc_now/0` för att få det aktuella datumet och tiden i UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Exempelutskrift:**
```
~U[2024-02-05 19:58:40.925931Z]
```

För att bara få det aktuella datumet kan du extrahera komponenterna för år, månad och dag:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Exempelutskrift:**
```
~D[2023-05-04]
```

### Användning av Timex-biblioteket
För mer komplexa datum-tidskrav kan ett populärt tredjepartsbibliotek kallat Timex användas. Lägg först till `Timex` till dina beroenden i mix.exs:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Efter att ha installerat beroendet (`mix deps.get`), kan du använda Timex för att få det aktuella datumet:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Exempelutskrift:**
```
~D[2023-05-04]
```

Timex erbjuder omfattande funktionaliteter för manipulation av datum och tid, vilket gör det till ett kraftfullt tillägg till dina Elixir-applikationer, särskilt när det handlar om tidszoner, formatering och tolkning av datum och tider.

Genom att förstå och utnyttja Elixirs inbyggda funktionalitet och Timex-biblioteket kan du enkelt arbeta med datum och tider i dina Elixir-applikationer, anpassa upplevelsen till ditt applikations behov med precision och lätthet.
