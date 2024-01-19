---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Muunnetaan päivämäärä merkkijonoksi tarkoittaa päivämäärätiedon esittämistä sanallisesti tai numeerisesti. Tätä tehdään usein, jotta päivämäärät voidaan esittää ihmisille ymmärrettävässä muodossa tai jotta ne voidaan lajitella tiedostoihin tai tietokantoihin. 

## Miten:

```Elixir
defmodule DateFormat do
  def convert_date_to_string(date) do
    date
    |> Date.to_iso8601 
  end
end
```
Esimerkki tulosteesta:
```Elixir
iex> DateFormat.convert_date_to_string(~D[2021-11-22])
"2021-11-22"
```

## Syvä sukellus

Historia: Elixirin Date-moduuli perustuu Erlangin kalenterimoduuliin, joka otettiin käyttöön Erlang 20:ssa. 

Vaihtoehdot: Käyttäjät voivat käyttää kolmannen osapuolen kirjastoja, kuten Timex, lisätaidoille, kuten aikavyöhykkeiden tuki. 

Toteutusyksityiskohdat: `Date.to_iso8601/1` -funktio muuntaa Date-tyyppisen argumentin ISO 8601 -päivämääräksi (merkkijono). Date-moduuli käyttää Gregoriaanista kalenteria.

## Katso myös

Elixirin Date-moduulin virallinen dokumentaatio: https://hexdocs.pm/elixir/Date.html

Timex, monipuolinen päivämäärä- ja kellonaikakirjasto: https://hexdocs.pm/timex/Timex.html

Tutustu Erlangin kalenterimoduuliin: https://erlang.org/doc/man/calendar.html