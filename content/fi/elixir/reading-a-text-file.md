---
title:                "Tiedostojen lukeminen"
html_title:           "Elixir: Tiedostojen lukeminen"
simple_title:         "Tiedostojen lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

Mitä ja miksi?
Tiedoston lukeminen on prosessi, jossa ohjelma lukee tietoja tiedostosta ja käsittelee niitä. Tämä on tärkeä osa ohjelmointia, sillä usein haluamme käsitellä tietoja, jotka on tallennettu tiedostoihin, kuten tekstitiedostoihin tai CSV-tiedostoihin.

Kuinka:
```Elixir
File.read("tiedostopolku.txt")
```
Tällä yksinkertaisella koodirivillä voimme lukea tekstitiedoston ja tallentaa sen muuttujaan. Voimme myös käyttää muita Elixirin tiedostonkäsittelytoimintoja, kuten ```File.stream!```, joka palauttaa tulostusvirran, tai ```File.write!```, jolla voimme kirjoittaa tietoja tiedostoon.

Syöte:
```
Tämä on esimerkkiteksti.
```
Ulostulo:
```
{:ok, "Tämä on esimerkkiteksti."}
```

## Syväsukellus:
Tekstitiedostojen lukeminen on ollut osa ohjelmointia jo pitkään. Ennen digitaalista aikakautta ohjelmoijat lukevat tietoja paperilta ja kirjoittivat ne käsin ohjelmakoodiin. Nykypäivänä tiedostojen lukeminen on tärkeää, sillä tiedot tallennetaan yleensä tiedostoihin, jotta niitä voidaan käyttää myös muiden ohjelmien kanssa.

On myös muita tapoja lukea tiedostoja Elixirissä, kuten käyttämällä ulkoisia kirjastoja, kuten ```FileParser``` tai ```FastCSV```. Näitä voi olla tarpeen käyttää, jos ohjelmaa täytyy lukea monimutkaisempia tiedostoja, kuten XML- tai JSON-muodossa olevia tiedostoja.

Kun Elixiriä käytetään web-sovelluksissa, tiedostojen lukeminen voi olla tarpeen, esimerkiksi käyttäjän yllä lähettämien kuvien tai dokumenttien käsittelyssä. Tällöin tiedoston lukeminen Tiedostojärjestelmä-moduulilla on hyödyllinen tapa käsitellä tiedostoja.

Edellä mainitut esimerkit ovat yksinkertaisia ja perustuvat oletuksiin onnistuneesta tiedostojen lukemisesta. Kuten aina ohjelmoinnissa, on tärkeää käsitellä myös virhetilanteet. Oikeanlaiset virhekäsittelymekanismit ja virheiden tarkka lukeminen ovat tärkeitä osia ohjelmiston kehityksessä.

## Katso myös:
Virallinen dokumentaatio Tiedostojärjestelmä-moduulista: https://hexdocs.pm/elixir/File.html