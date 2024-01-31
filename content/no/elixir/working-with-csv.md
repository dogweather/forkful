---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV (Comma-separated values) innebærer data separert med kommaer. Programmerere bruker CSV fordi det er et enkelt format for å lagre og dele store datamengder, som kan brukes på tvers av ulike systemer.

## Hvordan gjøre det:
Elixir gjør det enkelt å jobbe med CSV-filer ved bruk av eksterne biblioteker som `CSV`. Her er et eksempel:

```elixir
# For å inkludere CSV-biblioteket, legg til {:csv, "~> 2.4"} i mix.exs, og kjør mix deps.get

defmodule MyCSV do
  require CSV

  def read_csv(path) do
    File.stream!(path)
    |> CSV.decode(separator: ?;, headers: true)
    |> Enum.each(&process_row/1)
  end

  defp process_row(row) do
    IO.inspect(row)
  end
end

# Bruk
MyCSV.read_csv("data.csv")
```
Eksempelutskrift for en rad: `%{"Header1" => "Verdi1", "Header2" => "Verdi2"}`

## Dypdykk
CSV er ikke nytt; det stammer fra tidlige regneark i 1970-årene. Alternativer som JSON og XML tilbyr mer struktur og funksjonalitet, men er tyngre å parse. Elixir's bibliotek `CSV` tilbyr en lettvekts løsning, med fleksibel håndtering av tegnsett og støtte for kodning og dekodning av datastrømmer.

## Se Også
- Offisiell dokumentasjon for CSV-biblioteket i Elixir: [hexdocs.pm/csv](https://hexdocs.pm/csv/CSV.html)
- Elixir's offisielle side: [elixir-lang.org](https://elixir-lang.org/)
- Mer informasjon om CSV formatet: [RFC 4180](https://tools.ietf.org/html/rfc4180)
