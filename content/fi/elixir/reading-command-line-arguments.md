---
title:                "Käskynlinja-argumenttien lukeminen"
html_title:           "Elixir: Käskynlinja-argumenttien lukeminen"
simple_title:         "Käskynlinja-argumenttien lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Joskus ohjelmia halutaan suorittaa erilaisilla parametreilla riippuen esimerkiksi käyttötarkoituksesta tai ympäristöstä. Siksi on tärkeää tietää, miten lukea komentoriviparametreja Elixirilla.

## Miten
Komentoriviparametrien lukeminen Elixirilla on helppoa ja vaivatonta. Alla on esimerkki yksinkertaisesta koodista, joka lukee annetun komentoriviparametrin ja tulostaa sen:

```Elixir
defmodule Args do
  def print(arg) do
    IO.puts("Annettu parametri on: #{arg}")
  end
end

Args.print(System.argv()[0])
```

Jos annamme komentorivillä esimerkiksi seuraavanlaisen komennon: `elixir args.exs Hello`, tulostuu "Annettu parametri on: Hello".

Koodissa käytetään `System.argv()`-funktiota, joka palauttaa listan käyttäjän antamista komentoriviparametreista. Funktio palauttaa listan, joka sisältää myös itse ohjelman nimen ensimmäisenä parametrina. Siksi koodissa käytetään `[0]` indeksinä, jotta saadaan vain käyttäjän antama parametri.

## Syväsukellus
Komentoriviparametrien lukemisen lisäksi Elixirilla on myös mahdollista käsitellä komentoriviparametreja eri tavoin, kuten järjestää ne tai tarkistaa niiden määrä. Alla olevassa esimerkissä luodaan `Args`-moduuli, johon lisätään useita funktioita parametrien käsittelyä varten.

```Elixir
defmodule Args do
  def get_first(args) do
    Enum.at(args, 0)
  end

  def sort(args) do
    Enum.sort(args)
  end

  def count(args) do
    length(args)
  end
end

args = System.argv()
IO.puts("Ensimmäinen parametri: #{Args.get_first(args)}")
IO.puts("Järjestetyt parametrit: #{Args.sort(args)}")
IO.puts("Parametrien määrä: #{Args.count(args)}")
```

Tässä esimerkissä luodaan ensin `args`-muuttuja, johon tallennetaan käyttäjän antamat parametrit. Tämän jälkeen käytetään `Args`-moduulin funktioita niiden käsittelyyn. Saamme tulostuksena ensimmäisen parametrin, järjestetyt parametrit ja parametrien määrän.

## Katso myös
- Elixirin virallinen dokumentaatio komentoriviparametrien lukemisesta: https://hexdocs.pm/elixir/System.html#argv/0
- Kaksikielinen Elixir-oppikirja: https://www.gitbook.com/book/antweb/kirja-elixir/details/fi