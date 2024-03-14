---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:24.572929-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta tarkoittaa\
  \ tekstin, joka esitt\xE4\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4\xE4, muuntamista `Date`-\
  \ tai `DateTime`-objektiksi, jonka Ruby ymm\xE4rt\xE4\xE4.\u2026"
lastmod: '2024-03-13T22:44:57.096587-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta tarkoittaa tekstin,\
  \ joka esitt\xE4\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4\xE4, muuntamista `Date`- tai `DateTime`-objektiksi,\
  \ jonka Ruby ymm\xE4rt\xE4\xE4.\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa tekstin, joka esittää päivämäärää, muuntamista `Date`- tai `DateTime`-objektiksi, jonka Ruby ymmärtää. Ohjelmoijat tekevät tämän suorittaakseen toimenpiteitä, kuten vertailuja, laskelmia tai muotoiluja päivämäärille, jotka ovat yleisiä tehtäviä sovelluksissa, jotka käsittelevät aikataulutusta, analytiikkaa tai datan käsittelyä.

## Kuinka:
Rubyn vakiokirjasto tarjoaa suorat tavat jäsentää päivämäärät merkkijonoista käyttämällä `Date`- ja `DateTime`-luokkia. Tässä on miten teet sen käyttäen Rubyn sisäänrakennettuja metodeja:

```ruby
require 'date'

# Jäsenetään päivämäärä merkkijonosta
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime tarkempaa ajan esitystä varten
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

Jos haluat enemmän kontrollia tai käsitellä muotoja, joita `parse`-metodi ei ehkä suoraan ymmärrä, voit käyttää `strptime` (merkkijono parsii ajan), määrittäen muodon nimenomaisesti:

```ruby
# Käytettäessä strptime-metodia mukautetuille formaateille
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Kolmannen osapuolen kirjastojen käyttö:

Vaikka Rubyn sisäänrakennetut mahdollisuudet ovat tehokkaita, joskus saatat mieluummin käyttää kolmannen osapuolen kirjastoja lisäominaisuuksien tai yksinkertaisemman syntaksin takia. Yksi suosittu valinta on `Chronic`-gem luonnollisen kielen jäsentämiseen:

1. Lisää ensin Chronic Gemfileesi ja suorita `bundle install`:
```ruby
gem 'chronic'
```

2. Sitten käytä sitä näin:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('seuraava tiistai')
puts parsed_chronic
# Tulostus vaihtelee nykyisen päivämäärän mukaan; olettaen että jäsentäminen tapahtuu 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` on erittäin hyödyllinen käyttäjän syötteen kannalta, sillä se ymmärtää laajan valikoiman luonnollisen kielen päivämäärämuotoja, tehden siitä tehokkaan työkalun sovelluksille, jotka vaativat joustavaa päivämäärän syöttöä.
