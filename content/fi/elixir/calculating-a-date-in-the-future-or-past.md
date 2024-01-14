---
title:    "Elixir: Laskenta tulevaisuuden tai menneen päivämäärän arvioimiseksi"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Elixir on dynaaminen ja joustava ohjelmointikieli, jonka avulla voit luoda tehokkaita ja luotettavia sovelluksia. Yksi käytännöllinen toiminto, jonka Elixir tarjoaa, on päivämäärän laskeminen tulevaisuudesta tai menneisyydestä. Tässä blogikirjoituksessa kerromme, miksi tämä toiminto on hyödyllinen ja miten sitä voidaan käyttää.

## Miten

Voit laskea tulevaisuuden tai menneisyyden päivämääriä käyttämällä Elixirin `Calendar` moduulia. Tämä moduuli tarjoaa kattavan valikoiman toimintoja päivämäärien käsittelyyn.

Katsotaanpa ensin, miten voimme laskea päivämäärän tietyn määrän päiviä tulevaisuuteen. Käytämme `Date.add/2` -funktiota, joka ottaa ensimmäisenä parametrina päivämäärän ja toisena parametrina päivien määrän, jonka haluamme lisätä siihen. Esimerkiksi, jos meillä on päivämäärä "1.1.2021" ja haluamme lisätä siihen 5 päivää, käytämme seuraavaa koodia:

```elixir
iex> Date.add(~D[2021-01-01], 5)
~D[2021-01-06]
```

Saman tuloksen saamme myös käyttämällä `Date.add/3` -funktiota, joka ottaa ensimmäisenä parametrina päivämäärän, toisena parametrina päivien määrän ja kolmantena parametrina yksikön, johon haluamme lisätä päivät. Esimerkiksi, jos haluamme lisätä kuukauden päivämäärään "1.1.2021", käytämme seuraavaa koodia:

```elixir
iex> Date.add(~D[2021-01-01], 1, :month)
~D[2021-02-01]
```

Voimme myös laskea menneisyyden päivämääriä käyttämällä `Date.subtract/2` tai `Date.subtract/3` -funktioita. Nämä toimivat samalla tavalla kuin vastaavat lisäysfunktiot, mutta vähentävät päiviä tai yksiköitä päivämäärästä.

## Syvällinen sukellus

On myös mahdollista laskea päivämääriä tarkemmin käyttämällä `Date.add/3` ja `Date.subtract/3` -funktioita. Näiden funktioiden kolmas parametri määrittää, millä tarkkuudella päivämäärään lisätään tai vähennetään. Oletusarvoisesti käytetään päiviä (`:day`), mutta voimme myös käyttää muita yksiköitä, kuten viikkoja (`:week`), kuukausia (`:month`) tai vuosia (`:year`). Voimme myös käyttää useampia parametreja samaan aikaan, jolloin päivämäärään lisätään tai vähennetään monia yksikköjä.

Lisäksi Elixirin `Calendar` moduuli tarjoaa myös muita hyödyllisiä toimintoja päivämäärien tarkasteluun ja muokkaamiseen. Tutustu rohkeasti moduulin dokumentaatioon ja löydät lisää käyttökelpoisia toimintoja.

## Katso myös

- [Elixirin `Calendar` moduulin dokumentaatio](https://hexdocs.pm/elixir/master/Calendar.html)
- [Elixirin virallinen verkkosivusto](https://elixir-lang.org/)
- [Elixirin yhteisöfoorumi](https://