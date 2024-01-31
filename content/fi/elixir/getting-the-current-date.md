---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:14:14.046216-07:00
simple_title:         "Nykyisen päivämäärän hankkiminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän haku tarkoittaa nykyhetken päivämäärän selvittämistä ohjelmassa. Tämä on hyödyllistä lokituksessa, aikaleimoissa ja päivämääräriippuvaisten toimintojen hallinnassa.

## Kuinka:
```elixir
# Käytetään Elixirin sisäänrakennettua DateTime-moduulia
# Hankitaan nykyhetken päivämäärä ja aika UTC-muodossa
nykyhetki = DateTime.utc_now()

# Tulostetaan se näytölle
IO.inspect(nykyhetki)

# Määritellään myös aikavyöhyke, esim. Helsinki (+2 tai +3 UTC riippuen kesä- tai talviajasta)
helsinki_aikavyohyke = "Europe/Helsinki"

# Muunnetaan nykyhetki Helsinki-ajaksi käyttäen Calendar-moduulin toiminnallisuutta
helsinki_aika = nykyhetki |> DateTime.shift_zone!(helsinki_aikavyohyke)

# Tulostetaan Helsinki-ajan
IO.inspect(helsinki_aika)
```

Sample output:
```
# UTC-aika
~U[2023-04-02 12:34:56Z]

# Helsinki-aika
# Huom: Lähtökohta otaksuu että nyt on kesäaika
~U[2023-04-02 15:34:56+03]
```

## Syväsukellus
Elixirin `DateTime`-moduuli on osa Elixirin peruskirjastoa jo versiosta 1.3 lähtien. Elixir käyttää sisäisesti Erlangin aikatoiminnoita, mutta tarjoaa niiden käyttöön helpomman ja Elixiriin sopivamman rajapinnan. Aikavyöhykkeiden käsittelyyn `DateTime.shift_zone!` funktio hyödyntää tz-databasea, joten aikavyöhyketiedot ovat ajan tasalla. Vaihtoehtoisia kirjastoja aikasempien Elixiriin ei sisäänrakennettuja aikatoiminnallisuuksien kanssa on esimerkiksi Timex, mutta moderni Elixir sisältää jo kaiken tarpeellisen useimmille aikaan liittyville tehtäville.

## Katso Myös
- Elixirin viralliset DateTime-dokumentaatiot: https://hexdocs.pm/elixir/DateTime.html
- Elixirin viralliset Calendar-dokumentaatiot: https://hexdocs.pm/elixir/Calendar.html
- tz-database informaatio: https://www.iana.org/time-zones
- Timex-kirjaston GitHub-sivu: https://github.com/bitwalker/timex
