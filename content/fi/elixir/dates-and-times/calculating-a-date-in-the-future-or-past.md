---
date: 2024-01-20 17:28:34.558383-07:00
description: "Menneiden tai tulevien p\xE4ivien laskenta on p\xE4iv\xE4m\xE4\xE4rien\
  \ manipulointia. Sit\xE4 k\xE4ytet\xE4\xE4n esimerkiksi vanhentumisp\xE4ivien, tapahtumien\
  \ ajoitusten tai\u2026"
lastmod: '2024-03-13T22:44:56.240560-06:00'
model: gpt-4-1106-preview
summary: "Menneiden tai tulevien p\xE4ivien laskenta on p\xE4iv\xE4m\xE4\xE4rien manipulointia."
title: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## How to:
Elixirissä päivämäärien kanssa työskentely onnistuu `Date`-moduulin avulla. Katsotaan esimerkkiä:

```elixir
# Nykyinen päivämäärä
current_date = Date.utc_today()

# Viiden päivän kuluttua
five_days_later = Date.add(current_date, 5)
IO.inspect(five_days_later)

# Kolme päivää sitten
three_days_ago = Date.add(current_date, -3)
IO.inspect(three_days_ago)
```

Sample output:
```
~D[2023-03-14]
~D[2023-03-06]
```

## Deep Dive
Päivämäärien laskenta Elixirissä liittyy suoraan Erlangin aikakirjastoon. Historiallisesti, päivämäärien käsittely monissa kielissä on ollut monimutkaista. Elixirin `Date`-moduuli yksinkertaistaa tätä käyttäen hyväksi Erlangin vakaita toimintoja.

Vaihtoehtoisia kirjastoja, kuten `Timex`, tarjoavat vielä laajempia ominaisuuksia, mutta perustoiminnot löytyvät suoraan Elixirstä. Moduulissa käytetään `:calendar` Erlang-moduulin toimintoja ja se tukee ISO 8601 -standardia, joka on kansainvälisesti tunnustettu päivämäärien esittämisen standardi.

`Date.add/2`-funktio ottaa päivämäärän ja lisää siihen (tai vähentää, jos luku on negatiivinen) halutun määrän päiviä. `Date.utc_today/0` palauttaa nykyisen UTC-päivämäärän.

## See Also
- Elixirin virallinen dokumentaatio `Date`-moduulista: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Lisätiedot ISO 8601 -standardista: [https://en.wikipedia.org/wiki/ISO_8601](https://en.wikipedia.org/wiki/ISO_8601)
- `Timex`-kirjasto vaihtoehtoisia aikakäsittelyfunktioita varten: [https://hex.pm/packages/timex](https://hex.pm/packages/timex)
