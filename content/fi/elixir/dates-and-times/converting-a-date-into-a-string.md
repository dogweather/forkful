---
date: 2024-01-20 17:36:36.523744-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:56.238699-06:00'
model: gpt-4-1106-preview
summary: .
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

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
