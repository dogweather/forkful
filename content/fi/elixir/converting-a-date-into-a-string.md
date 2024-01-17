---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Elixir: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän konvertoiminen merkkijonoksi tarkoittaa päivämäärän muuttamista ymmärrettävään muotoon, kuten "ma 14.6.2021". Ohjelmoijat tekevät tämän yleensä, kun heidän täytyy esittää päivämäärä käyttäjälle tai tallentaa se tietokantaan.

## Miten:
```Elixir
import Timex

today = Date.utc_today()

DateTime.to_string(today, "{WDabbrev} {D.}-{M}-{YYYY}")
# "Ma 14.6.2021"

DateTime.to_string(today, "{D}.{M}.{YYYY}")
# "14.6.2021"
```

## Syventävää tietoa:
Päivämäärän konvertoiminen merkkijonoksi on ollut tärkeä osa ohjelmointia jo pitkään. Aikaisemmin ohjelmoijien täytyi käyttää monimutkaisia laskutoimituksia ja erityisiä formaatteja saadakseen päivämäärästä haluamansa muodon. Nykyään onneksi ohjelmointikielten standardikirjastot tarjoavat helppokäyttöisiä ratkaisuja tähän ongelmaan. Elixirissä voit käyttää Timex-kirjastoa, joka tarjoaa monipuolisia työkaluja päivämäärän ja ajan manipulointiin.

## Katso myös:
- [Elixir Timex-kirjasto](https://hexdocs.pm/timex/readme.html)
- [Käyttäjän antaman päivämäärän tarkistus Elixirissä](https://akoutmos.com/post/working-with-dates-and-times-in-elixir-the-zero-to-the-zeroth-day-of-christmas)