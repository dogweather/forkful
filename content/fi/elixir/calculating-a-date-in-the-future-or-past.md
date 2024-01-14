---
title:                "Elixir: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä."
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

On useita syitä, miksi haluat laskea tulevan tai menneen päivämäärän Elixir-ohjelmoidessa. Esimerkiksi voit käyttää tätä ominaisuutta laskemaan tulevaisuuden tapahtumia, kuten syntymäpäiviä tai laskemaan menneitä tapahtumia, kuten vuosipäiviä.

## Miten

Voit laskea tulevan tai menneen päivämäärän käyttämällä Elixirin `Date`-moduulia. Tämä moduuli tarjoaa useita käteviä toimintoja päivämäärien käsittelyyn. Alla on esimerkki siitä, kuinka voit laskea päivämäärän kaksi viikkoa tulevaisuuteen:

```Elixir
iex> Date.add(Date.today, 14)
{:ok, ~D[2021-04-16]}
```

Voit myös antaa negatiivisen luvun, jos haluat laskea menneen päivämäärän. Alla on esimerkki siitä, kuinka voit laskea päivämäärän yksi kuukausi taaksepäin:

```Elixir
iex> Date.add(Date.today, -1, :months)
{:ok, ~D[2021-02-16]}
```

## Syvempi sukellus

Laskeminen tulevien tai menneiden päivämäärien kanssa voi olla hieman monimutkaista, koska on otettava huomioon esimerkiksi karkausvuodet ja kuukausien eri pituudet. Onneksi Elixirin `Date`-moduuli huolehtii näistä asioista puolestasi. Voit myös käyttää muita toimintoja, kuten `Date.start_of_week` tai `Date.end_of_month` tarvittaessa.

Jos haluat syvällisempää tietoa päivämäärien käsittelystä Elixirissä, suosittelemme lukemaan virallisen dokumentaation `Date`-moduulista: https://hexdocs.pm/elixir/Date.html

## Katso myös

- https://hexdocs.pm/elixir/Date.html - Virallinen dokumentaatio Elixirin `Date`-moduulista
- https://elixirschool.com/en/lessons/basics/dates/ - Elixir-koulun opetusmateriaali päivämääristä Elixirissä