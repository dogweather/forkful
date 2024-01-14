---
title:                "Elixir: Nykyisen päivämäärän hakeminen"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

Oletko koskaan tarvinnut nykyistä päivämäärää Elixir-ohjelmassasi? Onko sinun tarvinnut laskea päiviä tai tarkistaa, mitä päivämäärä on tiettynä päivänä? Elixir tarjoaa helpon ja tehokkaan tavan saada nykyinen päivämäärä ohjelmaasi. Se on erityisen hyödyllinen sovelluksissa, jotka käsittelevät ajanjaksoja tai jotka haluavat tallentaa käyttäjän tekemiä toimintoja tiettyyn päivämäärään.

## Miksi

Kuten mainittu, Elixirin avulla voit helposti päästä käsiksi nykyiseen päivämäärään ohjelmasi aikana. Se voi myös olla hyödyllinen sovelluksissa, jotka tarjoavat ulkoisia rajapintoja, jotka vaativat aikastemppeleitä. Riippuen siitä mitä käsittelet, päivämäärän käyttö voi olla välttämätöntä joissain tilanteissa.

## Miten

Elixirissä päivämäärä voidaan hakea kahdella eri tavalla: `Date.utc_today/0` tai `DateTime.utc_now/0`. Ensimmäinen palauttaa päivämäärän `Date` muodossa, kun taas jälkimmäinen palauttaa päivämäärän ja ajan `DateTime` muodossa. Huomaa, että molemmat käyttävät UTC-aikavyöhykettä, joten sinun täytyy muuntaa se haluamaasi aikavyöhykkeeseen, mikäli se on tarpeen.

```elixir
# Date.utc_today/0
iex> Date.utc_today()
~D[2021-04-07]

# DateTime.utc_now/0
iex> DateTime.utc_now()
~U[2021-04-07 10:00:00Z]
```

Huomaa myös, että molemmat metodit hyväksyvät parametrina halutun aikavyöhykkeen merkkijonona, mikäli haluat muuntaa päivämäärän heti oikeaan aikavyöhykkeeseen.

## Syvällinen sukellus

Päivämäärän hakemisen taustalla Elixir käyttää Erlangin `calendar` moduulia. Tämä moduuli tarjoaa runsaasti työkaluja päivämäärien ja aikojen käsittelyyn. Esimerkiksi voit tarkistaa mikä päivä on tiettynä päivänä ja kuukautena käyttämällä `:calendar.date_to_day_of_the_week/3` funktiota.

```elixir
iex> :calendar.date_to_day_of_the_week(2021, 4, 7)
1
```

Tämä palauttaa numeron 1, koska 1. huhtikuuta 2021 oli torstai. Voit myös muuttaa päivämäärän haluamaasi muotoon `:calendar.format/2` funktiolla.

```elixir
iex> :calendar.format("{YYYY, MMMM dd}", ~D[2021-04-07])
"2021, April 07"
```

Nämä ovat vain muutamia esimerkkejä siitä, mikä kaikki on mahdollista päivämäärien käsittelyssä Elixirissä. Suosittelemme tutustumaan tarkemmin `calendar` moduuliin ja sen tarjoamiin funktioihin.

## Katso myös

- [Elixirin virallinen dokumentaatio päivämäärän käsittelyyn](https://hexdocs.pm/elixir/DateTime.html)
- [Kalenteri-moduulin dokumentaatio (englanniksi)](http://erlang.org/doc/man/calendar.html)