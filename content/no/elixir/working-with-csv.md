---
title:                "Å jobbe med csv"
html_title:           "Elixir: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
CSV står for Comma Separated Values og er en vanlig måte å organisere og lagre data på i tekstfiler. Programmører bruker CSV-filer for å lagre og arbeide med store mengder data på en enkel og strukturert måte. Dette gjør det enklere å importere og eksportere data til og fra ulike programmer og plattformer.

## Slik gjør du:
I Elixir kan du arbeide med CSV-data ved å bruke funksjoner fra standardbiblioteket ```File```, ```CSV``` og ```Stream```. For eksempel kan du lese en CSV-fil ved å bruke ```File.stream!/2``` og deretter manipulere dataene ved hjelp av ```Stream.map/2```. Her er et eksempel på hvordan du kan skrive om hver rad i en CSV-fil til en liste i Elixir:

```
file_path = "min_fil.csv"

File.stream!(file_path)
|> CSV.decode()
|> Stream.map(&IO.inspect/1)
|> Stream.run()
```

Dette vil ta hver rad i filen og skrive den ut til konsollen som en liste. Du kan også bruke funksjonene ```CSV.encode/1``` og ```File.write!/2``` for å skrive data til en CSV-fil.

## Dypdykk:
CSV er et populært filformat i dataverdenen og har vært i bruk siden 1972. Det finnes mange forskjellige måter å arbeide med CSV-data på, og Elixir tilbyr en enkel og effektiv måte å gjøre det på. Alternativer til Elixir inkluderer programmeringsspråk som Python og R, som også har biblioteker for å håndtere CSV-data.

Når du arbeider med CSV-data i Elixir, bør du være oppmerksom på at funksjonene i standardbiblioteket ikke er ment for store datamengder. Hvis du har store CSV-filer, bør du vurdere å bruke et bibliotek som jobber med dataen i minne, som for eksempel `csv`-pakken fra Hex.

## Se også:
Her er noen ressurser for å lære mer om å arbeide med CSV i Elixir:

- [Elixir Standardbiblioteket](https://hexdocs.pm/elixir/Kernel.html#CSV)
- [`csv`-pakken fra Hex](https://hex.pm/packages/csv)
- [Den offisielle CSV-spesifikasjonen](https://tools.ietf.org/html/rfc4180)