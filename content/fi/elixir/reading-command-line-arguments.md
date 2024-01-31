---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:55:43.658100-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Komennoriviparametrien lukeminen on tapa vastaanottaa käyttäjältä tietoja ohjelman suorituksen yhteydessä. Ohjelmoijat käyttävät tätä toimintoa mukauttamaan ohjelman käytöstä ilman, että koodia tarvitsee muuttaa.

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
