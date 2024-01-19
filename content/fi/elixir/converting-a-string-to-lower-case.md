---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Alustetaan merkkijonot pienaakkosilla, jolloin ne standardisoidaan muotoon, joka on vertailtavissa ilman herkkyyttä kirjainkoolla. Tämä on hyödyllistä esimerkiksi salasanojen, sähköpostiosoitteiden tai URL:ien käsittelyssä.

## Kuinka:
Elixiriin sisältyy funktio `String.downcase/1` jota käytetään merkkijonojen käsittelyyn:

```Elixir
iex> String.downcase("Hei Maailma")
"hei maailma"
```
Yläolevan esimerkin tulostuma on `"hei maailma"`.

## Syvempi Sukellus:
Elixirin `downcase`-tyyppinen merkkijonotunnus juontaa juurensa Erlang:iin, josta se peri iso osa funktioistaan. Merkkijonojen käsittelytapoja on lukuisia. Elixir tarjoaa myös `String.upcase/1` muuntaakseen merkkijonot isoiksi kirjaimiksi.

On myös tärkeää huomata, että `downcase` tehdään Unicode mukaisesti, koska Elixir tukee Unicodea oletuksena. Se tarkoittaa, että erikoismerkit ja diakriittiset merkit käsitellään oikein.

```Elixir
iex> String.downcase("ÅÄÖ")
"åäö"
```
## Katso Myös:
Lisäapua Elixirin merkkijonojen käsittelystä loytyy seuraavista lähteistä:
* Elixirin virallinen dokumentaatio: [String module](https://hexdocs.pm/elixir/String.html)
* Joe Armstrongin kirja: [Programming Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)