---
title:    "Elixir: Päivämäärän hankkiminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Elixir on monipuolinen ohjelmointikieli, joka tarjoaa monia hyödyllisiä toimintoja ohjelmoijille. Yksi näistä toiminnoista on nykyisen päivämäärän hakeminen. Miksi sitten haluaisit saada nykyisen päivämäärän? Päivämäärä on usein tärkeä osa ohjelmia, kuten laskutusohjelmia tai tapahtumakalentereita. Joten jatka lukemista oppiaksesi kuinka saada nykyinen päivämäärä käyttäen Elixir-kieltä.

## Kuinka tehdä se

Elixir tarjoaa helpon ja selkeän tavan hakea nykyinen päivämäärä. Voit käyttää Date-moduulia, joka sisältää toimintoja päivämäärän käsittelyyn. Käytä Date.utc_today/1 -toimintoa, mikä palauttaa nykyisen päivämäärän koordinoituna yleisessä koordinoimattomassa ajassa (UTC). Jos haluat päivämäärän oman aikavyöhykkeen mukaisena, voit käyttää Date.local_today/1 -toimintoa. Katso alla olevasta esimerkistä miten se tapahtuu:

```Elixir
# Hae nykyinen päivämäärä UTC-ajassa
Date.utc_today()
# Output: ~D[2020-11-12]

# Hae nykyinen päivämäärä omassa aikavyöhykkeessäsi
Date.local_today()
# Output: ~D[2020-11-12]
```

Voit myös muuttaa päivämäärän haluttuun muotoon Date.format/2 -toiminnolla. Esimerkiksi jos haluat päivämäärän muodossa "dd/mm/yyyy", voit käyttää seuraavaa koodia:

```Elixir
Date.format(Date.utc_today(), "dd/mm/yyyy")
# Output: "12/11/2020"
```

## Syventävä sukellus

Elixirin Date-moduuli käyttää Erlangin :calendar-moduulia päivämäärien käsittelyyn. Tämä tarkoittaa, että Date-moduuli perii kaikki toiminnot ja ominaisuudet :calendar-moduulista. :calendar-moduuli on täynnä hyödyllisiä toimintoja, kuten päivämäärän järjestäminen ja muuttaminen toiseen aikavyöhykkeeseen. Suosittelemme tutustumaan tarkemmin :calendar-moduulin dokumentaatioon saadaksesi lisätietoja.

Toinen asia, joka on hyvä huomioida, on päivämäärän tallennustyyppi Elixirissä. Kun käytät Date-moduulia, palautettu päivämäärä on Elixirin sisäänrakennettu Date-tietotyyppi. Tämä tarkoittaa, että voit suorittaa päivämäärään liittyviä toimintoja käyttämällä Elixirin sisäänrakennettuja funktioita, kuten Date.add/2 tai Date.diff/2.

## Katso myös

- [Date-moduulin dokumentaatio](https://hexdocs.pm/elixir/Date.html)
- [Elixirin sisäänrakennetut funktiot](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Erlangin :calendar-moduulin dokumentaatio](http://erlang.org/doc/man/calendar.html)