---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:53.069348-07:00
description: "CSV-tiedostojen (pilkuilla erotettujen arvojen) k\xE4sittelyyn kuuluu\
  \ tietojen lukemista ja kirjoittamista n\xE4ihin tiedostoihin, mik\xE4 on yleinen\
  \ tarve\u2026"
lastmod: 2024-02-19 22:05:15.190257
model: gpt-4-0125-preview
summary: "CSV-tiedostojen (pilkuilla erotettujen arvojen) k\xE4sittelyyn kuuluu tietojen\
  \ lukemista ja kirjoittamista n\xE4ihin tiedostoihin, mik\xE4 on yleinen tarve\u2026"
title: "Ty\xF6skentely CSV:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV-tiedostojen (pilkuilla erotettujen arvojen) käsittelyyn kuuluu tietojen lukemista ja kirjoittamista näihin tiedostoihin, mikä on yleinen tarve tehtävissä, jotka vaativat datan tuontia/vientiä tai yksinkertaisia tallennusratkaisuja. Ohjelmoijat hyödyntävät tätä toiminnallisuutta datan vaihtoon järjestelmien välillä, nopeaan datan muokkaukseen, tai tilanteissa, joissa kevyt ja helposti muokattava datamuoto on edullinen.

## Kuinka:

Elixir, tehokkaan kuviohakunsa ja putkituensa ansiosta, pystyy käsittelemään CSV-tiedostoja tehokkaasti, jopa ilman kolmannen osapuolen kirjastoja. Kuitenkin, edistyneempiin tarpeisiin `nimble_csv`-kirjasto on nopea ja suoraviivainen valinta.

### CSV-tiedoston lukeminen ilman ulkoisia kirjastoja

Voit lukea ja jäsentää CSV-tiedoston käyttämällä Elixiring sisäänrakennettuja funktioita:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Esimerkkikäyttö
CSVReader.read_file("data.csv")
# Tuloste: [["Otsikko1", "Otsikko2"], ["Rivi1Arvo1", "Rivi1Arvo2"], ["Rivi2Arvo1", "Rivi2Arvo2"]]
```

### Kirjoittaminen CSV-tiedostoon

Samankaltaisesti, jos haluat kirjoittaa tietoja CSV-tiedostoon:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# Esimerkkikäyttö
data = [["Otsikko1", "Otsikko2"], ["Arvo1", "Arvo2"], ["Arvo3", "Arvo4"]]
CSVWriter.write_to_file("output.csv", data)
# Luo output.csv:n datalla muotoiltuna CSV:ksi
```

### Käyttäen `nimble_csv`:tä

Monimutkaisempiin CSV-käsittelyihin, `nimble_csv` tarjoaa tehokkaan ja joustavan tavan työskennellä CSV-datan kanssa. Lisää ensin `nimble_csv` riippuvuuksiisi `mix.exs`-tiedostossa ja suorita `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

CSV-datan jäsentäminen `nimble_csv`:llä:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Esimerkkikäyttö
MyCSVParser.parse("data.csv")
# Tuloste `nimble_csv`:n kanssa voidaan mukauttaa määritelmän mukaan, mutta yleensä se näyttää listalta listoja tai tupleja riippuen siitä, kuinka olet asettanut jäsentimesi.
```

CSV-datan kirjoittaminen `nimble_csv`:llä vaatii manuaalista datasi muuntamista asianmukaiseen muotoon ja sitten sen kirjoittamista tiedostoon, paljon kuten pelkässä Elixirin esimerkissä mutta hyödyntäen `nimble_csv`:tä oikein muotoiltujen CSV-rivien tuottamiseen.

Valitsemalla sopivan lähestymistavan tehtäväsi monimutkaisuuden mukaan, voit käsitellä CSV-tiedostoja Elixirissa suurella joustavuudella ja voimalla.
