---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:47.401780-07:00
description: "Kuinka: Kartan luominen on suoraviivaista. K\xE4yt\xE4t `%{}`-syntaksia,\
  \ n\xE4in."
lastmod: '2024-03-13T22:44:56.219476-06:00'
model: gpt-4-0125-preview
summary: Kartan luominen on suoraviivaista.
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Kuinka:
Kartan luominen on suoraviivaista. Käytät `%{}`-syntaksia, näin:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Arvojen käyttäminen tapahtuu avainten avulla:

```elixir
IO.puts my_map["name"]
```
Tuloste: `Alex`

Arvojen lisäämiseen tai päivittämiseen, voit käyttää `Map.put/3`-funktiota:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Tuloste: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Avainten poistaminen on yhtä yksinkertaista `Map.delete/2` avulla:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Tuloste: `%{"location" => "NY", "name" => "Alex"}`

## Syväsukellus
Elixirin kartat ovat vanhempien avain-arvo-tallennustyyppien, kuten Rubyn Hashien tai Pythonin sanakirjojen, evoluutio. Ne mahdollistavat tehokkaammat haut ja lisäykset, tehden niistä modernin Elixir-ohjelmoinnin suosikkeja. On huomionarvoista, että ennen karttoja, Elixir käytti HashDict ja Dict -moduuleja, jotka ovat nyt vanhentuneita.

Kuitenkin, skenaarioita varten, jotka vaativat järjestettyä dataa, saatat katsoa Elixirin avainsanalistoja. Nämä ovat tuple-pareista koostuvia listoja, tehokkaita pienemmille kokoelmille, mutta eivät yhtä suorituskykyisiä suurille datamäärille kuin kartat.

Pidä mielessä, että kartat säilyttävät avaimensa "litteässä" rakenteessa, tehden suorista pääsyistä sisäkkäisiin arvoihin hieman hankalia. Syvään sisäkkäisyyteen, saatat harkita rakenteellista pääsyä `get_in`, `put_in`, `update_in`, ja `get_and_update_in` funktioiden kautta, jotka mahdollistavat dynaamisemman lähestymistavan sisäkkäisten tietojen käsittelyyn.

Yhteenvetona, vaikka kartat ovat go-to ratkaisusi assosiatiivisiin taulukkotarpeisiin Elixirissä, kieli tarjoaa rikkaan valikoiman tietorakenteita jokaiseen skenaarioon, rohkaisten sinua valitsemaan oikean työkalun tehtävään.
