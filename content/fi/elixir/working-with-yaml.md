---
title:                "Elixir: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi
Monet modernit ohjelmointikielet tarjoavat YAML-paketin tuekseen tiedostojen lukemiseen ja parsimiseen. Elixir ei ole poikkeus. YAML on kätevä tiedostomuoto, joka mahdollistaa tietojen tallentamisen selkeäksi ja helposti luettavaksi tekstimuotoon. Elixirissä työskennellessä YAML:n avulla voit helposti lukea ja tallentaa tiedostoja, jotka sisältävät monimutkaisia tietorakenteita.

## Kuinka
YAML-paketti on sisäänrakennettu Elixirin standardikirjastoon, joten sen käyttöönottoon ei tarvita erillistä asennusta. Voit alkaa käyttää sitä yksinkertaisesti lataamalla sen mix.exs-tiedostoon.

```
def deps do
  [{:yaml, "~> 0.0.1"}]
end
```

Seuraavaksi voit ladata tiedoston YAML-moduulilla ja käyttää siinä olevia funktioita tiedon lukemiseen ja tallentamiseen.

```
require YAML

# Tiedoston lukeminen
{:ok, data} = File.read("tiedosto.yml")
tallennettu_data = YAML.load(data)

# Tiedoston tallentaminen
tallennettu_data = %{"nimi" => "Suomi", "pääkaupunki" => "Helsinki"}
{:ok, tallennettu_tiedosto} = File.write("tiedosto.yml", YAML.dump(tallennettu_data))
```

Tiedoston lukemisessa ja tallentamisessa käytetään YAML-moduulin `load` ja `dump` -funktioita, jotka käsittelevät tiedon muuntamista tekstimuodosta Elixirin tietorakenteiksi ja päinvastoin.

## Syventävä tarkastelu

YAML-muodolla on monia hyödyllisiä ominaisuuksia, kuten mahdollisuus sisällyttää arvoihin kommentteja ja monitasoinen hierarkia. Tämän takia se on suosittu tiedostomuoto muun muassa konfiguraatiotiedostoissa.

Voit myös käyttää YAML-muotoa hyödyntämällä tietokantaa, kuten Mnesiaa tai PostgreSQL:ää, tallentamaan ja lukemaan tietoa. Voit käyttää tätä esimerkiksi silloin, kun haluat tallentaa tietorakenteen jota käytetään sovelluksessasi, mutta et halua käyttää perinteistä tietokantaa.

## Katso myös
- [Elixirin YAML-paketin dokumentaatio](https://hexdocs.pm/elixir/YAML.html)
- [YAML:n virallinen sivusto](https://yaml.org/)
- [Elixir-foorumin keskustelut YAML:stä](https://elixirforum.com/search?q=yaml)