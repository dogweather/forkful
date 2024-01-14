---
title:                "Elixir: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa joudutaan vertaamaan kahta päivämäärää keskenään. Tämä voi olla tarpeellista esimerkiksi tietokantojen kyselyissä tai ajankohtaisten tapahtumien tarkastelussa. Elixirilla tämä on helppoa ja tehokasta toteuttaa, joten on hyvä oppia miten se tehdään.

## Kuinka tehdä

Vertaamalla kahden päivämäärän välillä voimme saada selville kumpi niistä on aiempi, myöhempi tai ovatko ne samat. Elixirilla tämä onnistuu yksinkertaisesti käyttämällä Erlangin sisäänrakennettua `:calendar` moduulia ja sen funktiota `:calendar.compare/2`, joka vertailee kahta päivämäärää ja palauttaa tuloksen atomina.

```Elixir
date_1 = {:calendar, {2020, 9, 1}}
date_2 = {:calendar, {2020, 9, 15}}

:calendar.compare(date_1, date_2)
# => :lt
```

Esimerkissä luomme kaksi tuplia, jotka edustavat päivämääriä ja annamme ne funktiolle `:calendar.compare/2`. Funktion palauttama atomi kertoo meille, että ensimmäinen päivämäärä on edellä kuin toinen.

## Syvempi sukellus

`:calendar` moduulilla on muitakin hyödyllisiä funktioita, joita voimme käyttää päivämäärien käsittelyssä. Esimerkiksi `:calendar.date_to_gregorian_days/1` muuntaa päivämäärän päiviksi vuoden alusta, jolloin voimme vertailla päiviä keskenään. `:calendar.is_leap_year/1` kertoo meille onko annettu vuosi karkausvuosi.

Päivämäärien lisäksi Elixirilla on myös moduuli `DateTime`, joka tarjoaa laajempia ominaisuuksia päivämäärien ja aikaleimojen vertailuun. Lisäksi Elixir 1.9:n jälkeen on tullut mukaan uusi `Date` moduuli, joka tarjoaa lisää toimintoja päivämäärien kanssa työskentelyyn.

## Katso myös

- Elixirin virallinen dokumentaatio: [https://hexdocs.pm/elixir/Calendar.html](https://hexdocs.pm/elixir/Calendar.html)
- Miksi ja miten käyttäisin Elixiria? [https://www.juusoalasuutari.fi/2017/07/10/elixir-miksi-ja-miten](https://www.juusoalasuutari.fi/2017/07/10/elixir-miksi-ja-miten)
- Päivämäärien käsittelyn perusteet Elixirissä: [https://itnext.io/working-with-dates-in-elixir-7b1f1c222e3d](https://itnext.io/working-with-dates-in-elixir-7b1f1c222e3d)