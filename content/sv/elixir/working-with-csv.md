---
title:                "Arbeta med csv"
html_title:           "Elixir: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV, också känt som Comma-Separated Values, innebär att behandla data som är organiserad i tabeller med kolumner och rader. Det är vanligtvis ett enkelt sätt att dela och utbyta data, särskilt när man hanterar stora mängder information. Programvaror som Excel och databaser stöder också CSV-formatet. Därför är det viktigt att kunna läsa och skriva CSV-filer för att hantera data på ett effektivt sätt.

## Hur man:

```Elixir
defmodule CSV do
  # Skapar en funktion för att läsa en CSV-fil
  def read(file_name) do
    raw_data = File.read!(file_name)
    # Splittar raderna efter radbrytning
    lines = String.split(raw_data, [ch: 10])
    # Splittar raderna efter kommatecken och tar bort citattecken runt varje värde
    lines |> Enum.map(fn line -> String.split(line, [ch: 44]) |> Enum.map(fn x -> String.trim(x, "\"") end) end)
  end

  # Skapar en funktion för att skriva till en CSV-fil
  def write(file_name, data) do
    # Konverterar data till en sträng och lägger till rad- och kommatecken på rätt ställen
    output = data |> Enum.map(fn line -> Enum.join(line, ",") <> "\n" end) |> Enum.join
    File.write!(file_name, output)
  end
end

# Exempel på hur man kan använda funktionerna
data = CSV.read("min_data.csv")
CSV.write("ny_data.csv", data)

```

## Deep Dive:

CSV-formatet har funnits sedan 1972 och användes ursprungligen för tabellformat från IBM-mainframe. Sedan dess har det blivit ett populärt sätt att dela data mellan olika program och system. Alternativ till CSV inkluderar JSON och XML, men CSV är fortfarande mycket vanligt förekommande inom datahantering och utbyte. När man arbetar med CSV i Elixir är det viktigt att hantera eventuella speciella tecken som kan finnas i data, som till exempel radbrytningar eller kommatecken som inte är en del av värden.

## Se även:

- Elixir's officiella dokumentation om [String-modulen](https://hexdocs.pm/elixir/String.html) som innehåller funktioner för att arbeta med text och teckensträngar.
- [Elixir School](https://elixirschool.com/en/) en gratis läroplattform för att lära sig Elixir och dess ekosystem.