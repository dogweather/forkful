---
date: 2024-01-20 17:55:43.658100-07:00
description: "How to: Elixiriss\xE4 komennoriviparametrit saa napattua talteen `System.argv/0`\
  \ -funktiolla. Tarkastellaan esimerkki\xE4."
lastmod: '2024-03-13T22:44:56.242480-06:00'
model: gpt-4-1106-preview
summary: "Elixiriss\xE4 komennoriviparametrit saa napattua talteen `System.argv/0`\
  \ -funktiolla."
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to:
Elixirissä komennoriviparametrit saa napattua talteen `System.argv/0` -funktiolla. Tarkastellaan esimerkkiä:

```elixir
defmodule CommandLineDemo do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

CommandLineDemo.main()
```

Kun suoritat edellä mainitun skriptin komennolla `elixir demo.exs arg1 arg2 arg3`, saat tulosteeksi:

```
["arg1", "arg2", "arg3"]
```

## Deep Dive
Historiallisesti komennoriviparametrien käyttö on ollut tärkeä osa komentorivipohjaisten ohjelmien kanssa työskentelyssä. Elixirissä parametrit välitetään käyttöjärjestelmältä BEAM-ympäristöön, jossa Elixir suoritetaan. Tämä tapahtuu usein skriptin tai ohjelman käynnistyksen yhteydessä.

Vaihtoehtoja `System.argv/0`:lle ei ole suoranaisesti, mutta käsittelemiseen voit käyttää muita moduuleitakin. Esimerkiksi `OptionParser` on hyödyllinen, jos tarvitsee parsia liput ja kytkimet tarkemmin. Komentoriviparametrien lukemisessa keskeistä on, että ohjelma pystyy toimimaan joustavasti ilman että lähdekoodia tarvitsee muokata eri tilanteita varten.

## See Also
- Elixirin dokumentaatio komentoriviparametreista: https://hexdocs.pm/elixir/System.html#argv/0
- `OptionParser`-moduulin dokumentaatio: https://hexdocs.pm/elixir/OptionParser.html
- Elixirin opas komentorivi-sovellusten kirjoittamiseen: https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html
