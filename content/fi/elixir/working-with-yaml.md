---
title:                "Yaml-työskentely"
html_title:           "Elixir: Yaml-työskentely"
simple_title:         "Yaml-työskentely"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Miksi YAMLin käyttö on hyödyllistä Elixir-ohjelmoinnissa? YAML (Yet Another Markup Language) on helppolukuinen ja ihmisystävällinen tietojen esitysmuoto, joten se on hyödyllinen tiedostojen tallentamiseen sekä tietojen siirtoon eri ohjelmistojen välillä.

## Miten

YAMLin käyttö Elixirissä on yksinkertaista ja helppoa. Ensimmäiseksi meidän täytyy hankkia Elixirin YAML-kirjasto, joka sisältää tarvittavat työkalut YAML-tiedostojen lukemiseen ja kirjoittamiseen.

```Elixir
# Asennus
mix add yaml

# Tiedoston lukeminen
yaml = File.read!("tiedosto.yml")
data = Yaml.decode(yaml)

# Tiedoston kirjoittaminen
data = %{"nimi" => "Matti", "ika" => 25, "harrastukset" => ["luonnossa liikkuminen", "kirjallisuus"]}
yaml = Yaml.encode(data)
File.write!("henkilotiedot.yml", yaml)
```

Yllä olevassa esimerkissä määritellään ensin muuttuja `yaml` lukemaan YAML-tiedosto. Sitten käytetään `Yaml.decode`-funktiota muuntamaan YAML-muotoiset tiedot Elixirin avoimeen tietorakenteeseen. Tämän jälkeen muuttuja `data` sisältää tietojen kokonaisuuden, jota voidaan käsitellä Elixirissä normaalisti.

Seuraavassa osassa esimerkissä luodaan uusi YAML-tiedosto käyttäen `Yaml.encode`-funktiota ja tallennetaan se muuttujaan `yaml`. Tämän jälkeen `File.write!`-funktiolla kirjoitetaan tiedosto tallentaen `yaml`-muuttujan sisältö tiedostoon.

## Syvällinen sukellus

YAML-tiedostot koostuvat avain-arvo -pareista tai listoista, joita voidaan sisällyttää toisiinsa. Elixirin YAML-kirjasto käyttää Elixirin tietorakenteita vastaavasti. Esimerkiksi avain-arvo -parit muodostetaan `%{}` -muodossa, ja listat `[]` -muodossa.

YAML tukee myös monia muita ominaisuuksia, kuten ankkureita ja viittauksia, jotka mahdollistavat tietojen uudelleenkäytön ja lyhentävät tiedostojen kokoa. Elixirin YAML-kirjasto tukee myös näitä ominaisuuksia.

## Katso myös

- [Elixir - Dokumentaatio](https://hexdocs.pm/yaml/)
- [YAML - Virallinen verkkosivusto](https://yaml.org/)