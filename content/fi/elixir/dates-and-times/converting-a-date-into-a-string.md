---
title:                "Päivämäärän muuntaminen merkkijonoksi"
aliases:
- /fi/elixir/converting-a-date-into-a-string/
date:                  2024-01-20T17:36:36.523744-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Muuntaminen päivämäärästä merkkijonoksi tarkoittaa päivämäärä-datan esittämistä luettavassa muodossa. Kehittäjät tekevät tätä, koska ihmiset ymmärtävät paremmin "1. maaliskuuta 2023" kuin puhdasta aikaleimadataa.

## How to:
```elixir
# Luodaan Päivämäärä
date = ~D[2023-03-01]

# Muunnetaan merkkijonoksi
date_string = to_string(date)

# Tulostetaan merkkijono
IO.puts(date_string)
```
Tulostus:
```
"2023-03-01"
```

## Deep Dive
Elixir tarjoaa `Date`-moduulin päivämääräkäsittelyyn. Historiallisesti päivämäärän käsittely on ollut monimutkaista eri ohjelmointikielissä. Elixiriin tämä on tuotu käyttäjäystävällisesti BEAM-virtuaalikoneen, jolle Elixir on rakennettu, ominaisuuksien ansiosta.

Vaihtoehtoja on monia. Päivämäärän voi esittää eri formaateissa käyttäen `Timex`-kirjastoa, mikä on monipuolinen vaihtoehto Elixirin sisäänrakennetulle `Date`-moduulille. Tätä voi käyttää esimerkiksi `Timex.format!(date, "{ISO:Extended}")` esittämisen tapaan.

Perustoteutus Elixirissä käyttää `to_string/1` funktiota, joka hyödyntää sigil `~D` luodessaan `Date`-rakennetta ja muuntaessaan sen merkkijonoksi oletusmuodossa, kuten yllä olevassa esimerkissä on nähty.

## See Also
- Elixir `Date`-moduulin dokumentaatio: https://hexdocs.pm/elixir/Date.html
- Timex-kirjasto: https://hexdocs.pm/timex/Timex.html
- Elixirin sigilit: https://hexdocs.pm/elixir/master/sigils.html
