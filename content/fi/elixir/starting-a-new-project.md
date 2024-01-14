---
title:    "Elixir: Uuden projektin aloittaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Jos olet innokas ohjelmoija ja haluat haastaa itsesi uuden kielen parissa, Elixir voi olla juuri sinulle sopiva valinta. Elixir on funktionaalinen ohjelmointikieli, joka yhdistää korkean suorituskyvyn ja samanaikaisen ohjelmoinnin helposti luettavaan syntaksiin, joten se on loistava työkalu uuden projektin aloittamiseen.

## Kuinka aloittaa

Elixirin asentaminen on helppoa, ja voit tehdä sen suoraan komentoriviltä. Kun asennus on valmis, voit aloittaa uuden projektin seuraavasti:

```Elixir
mix new project_name
```

Tämä luo uuden kansiovakion nimeltä `project_name`, jossa on joitakin valmiita tiedostoja, kuten `mix.exs` ja `lib` kansio, jotka sisältävät projektisi koodin. Voit nyt aloittaa kirjoittamaan omaa koodiasi ja kääntää sen ajettavaan muotoon käyttämällä `mix` -komennon muunnelmia.

```Elixir
mix compile # Kääntää projektin koodin
mix run-iex # Käynnistää interaktiivisen Elixir konsolin
```

Voit myös suorittaa yksittäisiä tiedostoja käyttäen `elixir` komentoa, esimerkiksi:

```Elixir
elixir script.exs
```

## Syvempi sukellus

Elixir perustuu Erlang-virtuaalikoneeseen (BEAM), joten se hyödyntää sen ominaisuuksia, kuten saumoja (processes), joiden avulla voit käsitellä samanaikaisia prosesseja ilman manuaalista säiehallintaa. Tämä tekee Elixirista erinomaisen valinnan esimerkiksi web-sovellusten taustaprosessien hallintaan.

Lisäksi Elixir tarjoaa laajan valikoiman valmiita kirjastoja ja työkaluja, jotka helpottavat projektisi kehittämistä. Yhteisö on myös erittäin aktiivinen ja ystävällinen, joten apua ja neuvoja löytyy aina tarvittaessa.

## Katso myös

- [Elixirin viralliset sivut](https://elixir-lang.org/)
- [Elixirin dokumentaatio](https://hexdocs.pm/elixir)
- [Elixirin yhteisöfoorumi](https://elixirforum.com/)