---
title:                "Testien kirjoittaminen"
html_title:           "Elixir: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Testien kirjoittaminen on tärkeä osa ohjelmointia, joka auttaa varmistamaan koodin toimivuuden ja vähentää virheiden määrää. Testit ovat koodinpätkiä, jotka tarkistavat, toimiiko ohjelma kuten pitäisi ja palauttavat virheilmoituksia jos jotain menee pieleen. Ohjelmoijat kirjoittavat testejä varmistaakseen, että heidän koodinsa on luotettavaa ja toimii odotetulla tavalla.

## Miten:

```Elixir
defmodule Calculator do
  # Sum function
  def sum(x, y) do
    x + y
  end

  # Test for sum function
  test "sum" do
    assert Calculator.sum(1, 2) == 3
  end
```

Koodiesimerkissä näkyy, miten testit voidaan kirjoittaa ja liittää osaksi ohjelmakoodia. Testit kirjoitetaan käyttämällä ```test```-avainsanaa ja antamalla testille selkeä nimi. Testi suorittaa halutun toiminnon, tässä tapauksessa testataan, että sum-funktio palauttaa oikean tuloksen. Jos testi ei onnistu, se palauttaa virheilmoituksen.

## Syvemmälle:

Testien kirjoittaminen on saanut alkunsa jo vuosisatoja sitten, kun matemaatikot alkoivat käyttää todistusmenetelmiä varmistaakseen, että laskutoimitukset olivat oikein. Nykyään testaamisella on tärkeä rooli ohjelmistokehityksessä, ja ohjelmoijat käyttävät erilaisia kirjastoja ja työkaluja apunaan.

Yksi vaihtoehto on käyttää Behavior Driven Development (BDD) -menetelmää, joka keskittyy ohjelmakoodin kirjoittamiseen ensin testien avulla. Elixirissä BDD-menetelmään voi käyttää esimerkiksi kirjastoa nimeltä ExUnit. Testien kirjoittaminen on myös hyödyllistä sovellusten ylläpidossa, sillä se auttaa nopeasti havaitsemaan mahdolliset ongelmat ja varmistamaan, että muutokset eivät aiheuta uusia virheitä.

## Katso myös:

Lisätietoa testien kirjoittamisesta Elixirissä ja sen käytöstä löytyy seuraavista lähteistä:

- [Elixir ExUnit -virallinen dokumentaatio](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Codecademy: Intro to Testing in Elixir](https://www.codecademy.com/learn/learn-elixir/modules/elixir-testing-u)

Pidä huolta koodisi laadusta ja kirjoita testejä!