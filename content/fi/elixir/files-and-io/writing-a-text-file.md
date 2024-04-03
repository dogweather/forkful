---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:43.847714-07:00
description: "Kuinka: Elixir tekee tiedoston k\xE4sittelyst\xE4 yksinkertaista sis\xE4\
  \xE4nrakennettujen moduulien avulla. P\xE4\xE4asiallinen tapa kirjoittaa tiedostoon\
  \ on k\xE4ytt\xE4m\xE4ll\xE4\u2026"
lastmod: '2024-03-13T22:44:56.245224-06:00'
model: gpt-4-0125-preview
summary: "Elixir tekee tiedoston k\xE4sittelyst\xE4 yksinkertaista sis\xE4\xE4nrakennettujen\
  \ moduulien avulla."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Kuinka:
Elixir tekee tiedoston käsittelystä yksinkertaista sisäänrakennettujen moduulien avulla. Pääasiallinen tapa kirjoittaa tiedostoon on käyttämällä `File.write/2` tai `File.write!/2` funktioita, joista edellinen palauttaa `:ok` tai `:error` tuplen ja jälkimmäinen nostaa virheen epäonnistuessa.

Tässä on yksinkertainen esimerkki:

```elixir
# Kirjoitus tiedostoon, yksinkertainen viesti
File.write("hello.txt", "Hei, Maailma!")

# Kun ajat koodin, se luo 'hello.txt' tiedoston sisällöllä "Hei, Maailma!"
```

Liittäessäsi tiedostoon, käytät `File.open/3` funktiota `[:write, :append]` vaihtoehdoilla, sitten `IO.binwrite/2` funktiota sisällön liittämiseen:

```elixir
# Liitäminen tiedostoon
{:ok, tiedosto} = File.open("hello.txt", [:write, :append])
IO.binwrite(tiedosto, "\nLisätään toinen rivi.")
File.close(tiedosto)

# Nyt 'hello.txt' sisältää toisen rivin "Lisätään toinen rivi."
```

Jos työskentelet suurilla datamäärillä tai tarvitset enemmän kontrollia kirjoitusprosessiin, saatat käyttää `Stream` moduulia laiskaasti kirjoittaaksesi tiedoston dataa:

```elixir
# Suuren datamäärän kirjoittaminen laiskasti
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Numero: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn tiedosto ->
  Enum.each(stream_data, fn rivi ->
    IO.write(tiedosto, rivi)
  end)
end)

# Tämä luo 'numbers.txt', kirjoittaen numerot 0 - 9, kukin omalle rivilleen.
```

Projekteille, jotka vaativat tarkempaa tiedostonkäsittelyä, saatat tutkia kolmannen osapuolen kirjastoja, kuten `CSV`, joka tarjoaa räätälöityä toiminnallisuutta CSV-tiedoston käsittelyyn, mutta muista, että moniin tarkoituksiin Elixiring sisäänrakennetut kyvyt ovat enemmän kuin riittävät.
