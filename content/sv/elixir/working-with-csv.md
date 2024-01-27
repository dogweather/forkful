---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, eller "Comma-Separated Values", är ett enkelt filformat för att lagra tabulär data. Programmerare använder det för att enkelt utbyta data mellan olika program eller system.

## How to:
I Elixir använder vi `CSV` biblioteket för att hantera CSV-data. Så här ser det ut:

```Elixir
# Lägg till CSV som ett beroende i mix.exs
defp deps do
  [
    {:csv, "~> 2.4"}
  ]
end

# Läs en CSV-fil
{:ok, data} = File.read("exempel.csv")
rows = CSV.decode(data) |> Enum.to_list()

# Skriv en CSV-fil
headers = ["name", "age", "city"]
people = [["Alice", 34, "Stockholm"], ["Bob", 28, "Göteborg"]]
CSV.encode([headers | people]) |> Enum.each(&File.write!("output.csv", &1))
```

Exempeloutput för `output.csv`:
```
name,age,city
Alice,34,Stockholm
Bob,28,Göteborg
```

## Deep Dive
CSV har har använts sedan 1970-talet, vilket gör det till ett av de mest etablerade formaten för dataportabilitet. Alternativ inkluderar JSON och XML, som är mer flexibla men också mer komplexa. CSV i Elixir hanteras ofta via tredjepartsbibliotek som `CSV`, som exempelvis använder `nimble_csv` under huven för att få bättre prestanda.

## See Also
- Elixir CSV bibliotek: https://hex.pm/packages/csv
- Elixir officiella hemsida: https://elixir-lang.org/
- Programmering med CSV i andra språk: https://en.wikipedia.org/wiki/Comma-separated_values
