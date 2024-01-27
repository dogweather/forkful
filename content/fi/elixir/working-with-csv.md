---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV (Comma-Separated Values) on yksinkertainen tiedostomuoto datan tallentamiseen. Ohjelmoijat käyttävät CSV:tä, koska se on helppolukuinen ja yhteensopiva useiden ohjelmistojen ja kielten kanssa.

## Miten:

Elixirissä CSV-tiedostojen käsittelyyn voi käyttää CSV-kirjastoa, joka helpottaa rivien lukemista ja kirjoittamista.

```elixir
# Lisää csv-kirjasto mix.exs-tiedostoon
defp deps do
  [
    {:csv, "~> 2.4"}
  ]
end

# Lue CSV-tiedostoa
def read_csv(file_path) do
  File.stream!(file_path)
  |> CSV.decode(separator: ?;)
  |> Enum.each(&process_row/1)
end

# Prosessoi jokaista CSV-riviä
defp process_row(row), do: IO.inspect(row)

# Kirjoita CSV-tiedostoon
def write_csv(file_path, data) do
  CSV.encode_to_iodata(data, separator: ?;)
  |> :file.write(file_path)
end
```

Esimerkki lähdöstä:

```elixir
read_csv("path/to/your/file.csv")
# Tulostaisi jokaisen rivin, esim.
# ["ID", "Nimi", "Sähköposti"]
# ["1", "Matti Meikäläinen", "matti@example.com"]

write_csv("path/to/your/new_file.csv", [["ID", "Nimi", "Sähköposti"], ["2", "Liisa Virtanen", "liisa@example.com"]])
# Luo uuden CSV-tiedoston rivillä ID, Nimi, Sähköposti ja toisella rivillä tiedot 2, Liisa Virtanen, liisa@example.com
```

## Syväsukellus:

CSV-muoto on ollut käytössä jo vuosikymmeniä, tehden siitä lähes universaalin vaihtoehdon tabulaarisen datan jakamiseen. Json ja XML ovat moderneja vaihtoehtoja, mutta niiden rakenne on monimutkaisempi. Elixir käyttää pattern matching -ominaisuutta, joka tekee CSV-tiedostojen käsittelystä joustavampaa; voit esimerkiksi erottaa otsikkorivet helposti datasta.

## Katso Myös:

- Elixirin viralliset dokumentit: https://elixir-lang.org/docs.html
- CSV-kirjasto Elixirissä: https://hex.pm/packages/csv
- Ecto, Elixirin tietokanta-apuohjelma, joka tukee myös CSV: https://hexdocs.pm/ecto/Ecto.html
