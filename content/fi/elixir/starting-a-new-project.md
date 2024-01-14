---
title:    "Elixir: Uuden projektin aloittaminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi: Miksi aloittaa uusi projekti?

Monilla kehittäjillä tulee aika ajoin tarve aloittaa uusi projekti. Tämä voi johtua esimerkiksi uuden teknologian käytön halusta tai vanhan projektin päivittämisen tarpeesta. Elixir on suosittu ohjelmointikieli, joka tarjoaa monia etuja ja ominaisuuksia uuden projektin luomiseen.

## Miten: Esimerkkejä koodin kirjoittamisesta

Jos olet kiinnostunut aloittamaan uuden projektin Elixirillä, tässä on muutama esimerkki miten voit aloittaa. Ensinnäkin, voit luoda uuden Elixir-projektin käyttämällä "mix new" -komentoa. Tämä luo uuden kansiojrakenteen projektillesi ja sisältää tarvittavat alustavat tiedostot. Voit myös käyttää "mix new <nimi>" -komentoa, jonka avulla voit antaa projektillesi nimen.

```Elixir
mix new projekti
...
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/projekti.ex
* creating test
* creating test/test_helper.exs
* creating test/projekti_test.exs
```

Kun projekti on luotu, voit aloittaa koodaamisen! Voit käyttää Elixirin ominaisuuksia, kuten moduleita ja funktioita, helposti ja sujuvasti. Tässä esimerkissä luomme yksinkertaisen funktion, joka kertoo "Hei maailma!".

```Elixir
defmodule Hei do
  def hei_maailma do
    IO.puts "Hei maailma!"
  end
end
```

Kun suoritat tämän funktion, näet tuloksen:

```Elixir
iex> Hei.hei_maailma
Hei maailma!
:ok
```

## Syvällisemmin: Tietoa uuden projektin aloittamisesta

Aloittaessasi uuden projektin Elixirillä, on hyvä pitää mielessä muutamia asioita. Ensinnäkin, voit käyttää "mix" -työkalua moniin toimintoihin, kuten testien suorittamiseen ja koodin kääntämiseen. Lisäksi voit käyttää Ecto-kirjastoa tietokantayhteyksiin ja Phoenix-kehystä web-sovellusten luomiseen.

On myös tärkeää suunnitella projektisi rakennetta ja miettiä mitä toiminnallisuuksia se tarvitsee. Elixirin modulaarinen rakenne mahdollistaa projektin helpomman hallinnan ja ylläpidon. Kannattaa myös tutustua Elixirin syntaksiin ja käytäntöihin, jotta voit hyödyntää sen tarjoamia ominaisuuksia täysimääräisesti.

## Katso myös

- [Elixirin virallinen sivusto](https://elixir-lang.org/)
- [Elixirin dokumentaatio](https://hexdocs.pm/elixir/)
- [Ecto-dokumentaatio](https://hexdocs.pm/ecto/Ecto.html)
- [Phoenix-dokumentaatio](https://hexdocs.pm/phoenix/overview.html)