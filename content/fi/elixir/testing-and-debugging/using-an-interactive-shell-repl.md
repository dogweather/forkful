---
date: 2024-01-26 04:13:31.994173-07:00
description: "Kuinka: IEx:n k\xE4ynnist\xE4miseksi avaa terminaali ja kirjoita `iex`.\
  \ T\xE4ss\xE4 maistiainen."
lastmod: '2024-03-13T22:44:56.228555-06:00'
model: gpt-4-0125-preview
summary: "IEx:n k\xE4ynnist\xE4miseksi avaa terminaali ja kirjoita `iex`."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Kuinka:
IEx:n käynnistämiseksi avaa terminaali ja kirjoita `iex`. Tässä maistiainen:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Tulosteessa tulisi näkyä muuttujan asettaminen, funktion tulokset ja nimeämätön funktio työssään.

## Syväsukellus
IEx-komentotulkki on ollut osa Elixiria sen alkupäivistä lähtien. José Valim, Elixirin luoja, sai inspiraation toisten kielten interaktiivisista komentotulkeista, kuten Pythonin `python` ja Rubyn `irb`. Vaikka IEx jakaa monia ominaisuuksia näiden kanssa, se on suunniteltu käsittelemään Elixirin samanaikaisuutta ja on täysin integroitu Erlang VM:n ominaisuuksiin.

Vaihtoehtoja IEx:lle Erlangin ekosysteemissä on esimerkiksi `erl`, Erlangin komentotulkki. Mutta IEx tarjoaa Elixir-ystävällisemmän ympäristön, ominaisuuksia kuten kattava välilehden täydennys, historia ja apuohjelmat mukaan lukien.

IEx REPL on enemmän kuin leikkipaikka; se voi yhdistää saumattomasti käynnissä olevaan järjestelmään. Tämä on elintärkeää live-sovellusten vianetsinnässä. Taustalla oleva toteutus perustuu BEAM:iin (Erlang VM), varmistaen, että ominaisuudet kuten kuuman koodin vaihto tuetaan suoraan komentotulkissa.

## Katso Myös
Tarkista nämä lisälukemista ja resursseja varten:

- [Elixirin IEx-dokumentaatio](https://hexdocs.pm/iex/IEx.html)
- [Interaktiivinen Elixir (IEx) - Elixirin komentotulkki](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlangin `erl` dokumentaatio](http://erlang.org/doc/man/erl.html)
- [Elixirin interaktiivisen komentotulkin oppiminen](https://elixirschool.com/en/lessons/basics/iex_helpers/)
