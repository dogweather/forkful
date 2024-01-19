---
title:                "Päivämäärän lukeminen merkkijonosta"
html_title:           "Elixir: Päivämäärän lukeminen merkkijonosta"
simple_title:         "Päivämäärän lukeminen merkkijonosta"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

```markdown
## Mitä & Miksi
Stringin päivämääräparsinta tarkoittaa päivämäärän erottamista merkkijonoista. Ohjelmoijat tekevät tämän, koska on tärkeää ymmärtää ja käsitellä päivämääriä tiedonkäsittelyssä.

## Näin teet
Elixirissä voit käyttää `Date.from_iso8601/2`-funktiota stringin päivämääräparsintaan. Tässä esimerkki sen käytöstä:

```Elixir
dt_string = "2023-10-15"
{:ok, date} = Date.from_iso8601(dt_string)
IO.puts(day)
```
Tämän koodin odotetaan antavan seuraavan tulosteen:
```
~D[2023-10-15]
```

## Syvempi syöksy
Elixirin `Date.from_iso8601/2` perustuu kansainväliseen ISO 8601 -päivämääräformaattiin, joka on ollut olemassa yli 30 vuotta. Se on tärkeä, koska se tekee päivämäärien ymmärtämisen helpoksi kaikille, riippumatta heidän sijainnistaan tai kulttuuristaan.

Vaihtoehtoisesti voit käyttää myös muita kirjastoja, kuten `Timex` tai `Calendar`, jos haluat enemmän joustavuutta päivämääräformaattien suhteen. 

`Date.from_iso8601/2`:n toteutuksessa on kaksi osaa, ensimmäinen osa tarkistaa, onko annettu stringi hyväksyttävässä ISO 8601 formaatissa ja toinen osa palauttaa päivämäärän erityisessä formaatissa.

## Lisätietoja
1. Elixirin virallinen Date.from_iso8601 dokumentaatio: https://hexdocs.pm/elixir/Date.html#from_iso8601/2
2. Timex-kirjasto: https://hexdocs.pm/timex/Timex.html
3. Calendar-kirjasto: https://hexdocs.pm/calendar/Calendar.html
```