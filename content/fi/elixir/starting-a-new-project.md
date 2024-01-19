---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Uuden projektin aloittaminen tarkoittaa uuden ohjelmistotuotteen tai -palvelun luomista alusta alkaen. Ohjelmoijat tekevät tämän ideoinnistaan, oppimisestaan tai liiketoiminnan tarpeiden täyttämiseksi.

## Kuinka Tehdä:

Aloitetaan asentamalla Elixir kieli. Asenna se seuraavalla koodilla:

```elixir
sudo apt-get install elixir
```

Kun sinulla on Elixir asennettuna, voit luoda uuden projektin seuraavalla koodilla:

```elixir
mix new hello_world
```

Tämä luo uuden projektin nimeltä `hello_world`, joka luo seuraavat tiedostot ja hakemistot:

```elixir
* creating README.md
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/hello_world.ex
* creating test
* creating test/test_helper.exs
* creating test/hello_world_test.exs
```

## Sukellus syvemmälle

Uuden projektin aloittaminen on perusta kaikelle ohjelmoinnille. Jo 1960-luvulta lähtien ohjelmoijat ovat luoneet uusia projekteja järjestelmien rakentamiseksi.

Mix on Elixiriin sisältyvä rakennustyökalu. Se on moderni ja tehokas vaihtoehto monille vanhemmille työkaluille, kuten make tai ant. Mixiin sisältyy myös paketinhallinta Hexin kanssa.

Projektisi ensimmäinen tiedosto, `mix.exs`, määrittää projektin asetukset ja riippuvuudet. `lib/hello_world.ex` on pääohjelmatiedosto, joka sisältää runko-ohjelmasi.

## Katso myös

Lue lisää Elixiristä ja projektien luomisesta seuraavista lähteistä:

1. [Elixirin Virallinen Sivusto](https://elixir-lang.org/)
2. [Elixir School](https://elixirschool.com/)
3. [Phoenix Framework](https://phoenixframework.org/)
4. [Hex](https://hex.pm/)
5. [Awesome Elixir](https://github.com/h4cc/awesome-elixir)