---
title:                "Elixir: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför du bör arbeta med CSV

CSV (Comma Separated Values) är en vanligt förekommande filtyp som används för att lagra data i ett tabellformat. Detta gör det enkelt att dela och överföra data mellan olika system och program. Genom att lära dig att arbeta med CSV i Elixir kan du förenkla ditt arbete och förbättra din effektivitet.

## Så här gör du

För att arbeta med CSV i Elixir behöver du först installera ett bibliotek som heter `csv`. Detta kan göras genom att lägga till `{:csv, "~> 2.0"}` till din `mix.exs` fil och köra `mix deps.get` i terminalen.

När biblioteket är installerat kan du börja läsa och skriva CSV-filer med hjälp av några enkla kodexempel. Nedan följer ett exempel på hur du läser en CSV-fil och printar ut varje rad till konsolen:

```Elixir
alias CSV
"file.csv"
|> CSV.parse(headers: true)
|> Enum.each(fn line ->
  IO.puts "#{line["column1"]} - #{line["column2"]}"
end)
```

Detta kodexempel förutsätter att din CSV-fil har en rad med rubriker i början, vilket kan specificeras med `headers: true` argumentet i `CSV.parse` funktionen.

För att skriva data till en CSV-fil kan du använda `CSV.encode` funktionen tillsammans med `File.write` funktionen. Nedan följer ett exempel på hur du kan skapa en CSV-fil med två kolumner och två rader:

```Elixir
alias CSV
CSV.encode([
  ["Name", "Age"],
  ["John", 30],
  ["Lisa", 25]
])
|> File.write("new_file.csv")
```

Det är viktigt att notera att `CSV.encode` funktionen förväntar sig en lista av listor som argument, där varje lista representerar en rad i din CSV-fil.

## Djupdykning

Att arbeta med CSV i Elixir är mycket flexibelt och kan anpassas efter dina specifika behov. Du kan bland annat läsa och skriva CSV-filer med olika separatorer, ange avvikande radavslutare och anpassa kodningsformatet för olika språk. För en mer detaljerad information om alla möjligheter och funktioner i `csv` biblioteket, rekommenderar vi att läsa dokumentationen som finns tillgänglig online.

## Se även

- [csv biblioteket](https://github.com/beatrichartz/csv)
- [Elixir dokumentationen](https://hexdocs.pm/elixir/)