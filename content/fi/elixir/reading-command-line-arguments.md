---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komennon riviparametrien lukeminen tarkoittaa ohjelmalle syötettyjen käyttäjän antamien argumenttien vastaanottamista. Ohjelmoijat tekevät tämän, jotta voivat manipuloida ohjelman toimintaa käyttäjän tarpeiden mukaan.

## Kuinka:

Näin voit lukea komentorivin argumentit Elixirissä:

```Elixir
defmodule Demo do
  def main(args) do
    IO.inspect(args)
  end
end

System.argv |> Demo.main
```
Jos suoritat ohjelman komennolla `elixir demo.exs arg1 arg2`, saatu tulostus olisi `["arg1", "arg2"]`.

## Syvällisemmin:

Elixir periytyy alkuperäisestä Erlangista, joka rakennettiin vuonna 1986, ja on aina tukenut komentoriviparametreja.

Vaihtoehtoja komentoriviparametrien lukemiseen sisältävät bash- tai zsh-kuoret, Python, Ruby, Perl ja monet muut. Elixir tarjoaa kuitenkin funktionaalisella ohjelmointityylillään selkeän ja minimalistisen tavan tehdä tämä.

Elixirin komentoriviparametrien käsittely tapahtuu `System.argv` funktion kautta. Arvot palautetaan aina listana merkkijonoja. Niitä ei voi muuttaa sen suorituksen aikana, joka on kutsunut `main` funktion.

## Katso myös:

1. [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/System.html#argv/0)
3. [Erlang and OTP in Action](http://shop.oreilly.com/product/9781933988788.do):
Erlangin historiasta ja toteutuksesta, sisältää tietoa komentoriviparametrien käsittelystä.