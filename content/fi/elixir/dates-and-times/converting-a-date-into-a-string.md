---
date: 2024-01-20 17:36:36.523744-07:00
description: "Muuntaminen p\xE4iv\xE4m\xE4\xE4r\xE4st\xE4 merkkijonoksi tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4-datan esitt\xE4mist\xE4 luettavassa muodossa. Kehitt\xE4\
  j\xE4t tekev\xE4t t\xE4t\xE4, koska ihmiset ymm\xE4rt\xE4v\xE4t\u2026"
lastmod: '2024-03-11T00:14:30.169156-06:00'
model: gpt-4-1106-preview
summary: "Muuntaminen p\xE4iv\xE4m\xE4\xE4r\xE4st\xE4 merkkijonoksi tarkoittaa p\xE4\
  iv\xE4m\xE4\xE4r\xE4-datan esitt\xE4mist\xE4 luettavassa muodossa. Kehitt\xE4j\xE4\
  t tekev\xE4t t\xE4t\xE4, koska ihmiset ymm\xE4rt\xE4v\xE4t\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
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
