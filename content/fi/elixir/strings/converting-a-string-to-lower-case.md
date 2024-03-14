---
date: 2024-01-20 17:38:03.153578-07:00
description: "Muuttaminen merkkijono pienaakkosiksi tarkoittaa kirjainten muuttamista\
  \ kirjainkoosta riippumattomiksi. Koodarit tekev\xE4t t\xE4m\xE4n yleens\xE4 tekstin\
  \ vertailua\u2026"
lastmod: '2024-03-13T22:44:56.213548-06:00'
model: gpt-4-1106-preview
summary: "Muuttaminen merkkijono pienaakkosiksi tarkoittaa kirjainten muuttamista\
  \ kirjainkoosta riippumattomiksi. Koodarit tekev\xE4t t\xE4m\xE4n yleens\xE4 tekstin\
  \ vertailua\u2026"
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
---

{{< edit_this_page >}}

## Mikä & Miksi?
Muuttaminen merkkijono pienaakkosiksi tarkoittaa kirjainten muuttamista kirjainkoosta riippumattomiksi. Koodarit tekevät tämän yleensä tekstin vertailua tai järjestämistä varten, poistaen ongelmat erilaisista kirjainkoista.

## Kuinka:
Elixirissä muunnat merkkijonon pienaakkosiksi `String.downcase/1` funktiolla. Tässä esimerkit ja tulosteet.

```elixir
# Perusesimerkki
pienet_kirjaimet = String.downcase("Moi Maailma!")
IO.puts(pienet_kirjaimet)  # tulostaa "moi maailma!"

# Käytä kanssa erikoismerkkejä
erikoiset_pieniksi = String.downcase("ÅÄÖ Hei!")
IO.puts(erikoiset_pieniksi)  # tulostaa "åäö hei!"
```

## Syväsukellus
Elixir käyttää Unicodea merkkien käsittelyyn, mikä tarkoittaa, että `String.downcase/1` käsittelee oikein myös erikoismerkkejä, kuten ääkköset. Historiallisesti merkkijonojen käsittelyn tarve pienaakkosiksi nousi esille, kun vertailut ja lajittelut piti suorittaa yhdenmukaisesti. Vaihtoehtoisesti jotkut kielet tukevat `.lower()`-metodia tai vastaavia funktioita. Elixirissä erityisesti, suorituskyky ja toiminnallisuus tulevat Erlangin perässä, mikä tekee prosessista tehokkaan ja luotettavan.

## Katso Myös
- Elixirin dokumentaatio `String.downcase/1`: https://hexdocs.pm/elixir/String.html#downcase/1
- Unicode standardi ja Elixir: https://unicode.org
- Elixir School, merkkijonojen käsittelyä: https://elixirschool.com/en/lessons/basics/strings
