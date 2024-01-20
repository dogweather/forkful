---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen luominen on prosessi, jossa luodaan luku tai numeroryhmä, jolla ei ole mitään ennalta määrättyä järjestystä tai mallia. Ohjelmoijat käyttävät satunnaislukuja monissa tilanteissa, kuten simulointien, pelien jne. tekemisessä, jossa tarvitaan odottamattomia tuloksia.

## Miten:

Voit generoida satunnaislukuja seuraavasti Elixirissä:

```elixir
random_number = :rand.uniform(10)
IO.inspect(random_number)
```

Tämä koodi tuottaa satunnaisen kokonaisluvun väliltä 1 ja 10.

## Syvä sukellus:

Historiallisesti tietokoneet ovat aina tuottaneet pseudosatunnaislukuja, joita ei voida ennustaa. Itse asiassa tietokoneet eivät koskaan todella luo satunnaisia lukuja, mutta lukuja, jotka näyttävät tarpeeksi satunnaisilta, jotta niitä voida käyttää.

Elixir käyttää Erlangin :rand-moduulia satunnaisliukujen luomiseksi. Se on nopea ja alkaa tuottaa lukuja heti, kun se on käynnistetty ilman tarvittavaa erityistä alustusta.

Vaihtoehtoisesti, jos tarvitset tiukempaa satunnaisuutta, voit myös käyttää :crypto.strong_rand_bytes -toimintoa. Muista kuitenkin, että tämä käyttää enemmän laskentatehoa.

```elixir
random_bytes = :crypto.strong_rand_bytes(4)
:binary.decode_unsigned(random_bytes)
```

Tässä tapauksessa koodi luo satunnaisen 4 tavun pituisen arvon ja muuntaa sen kokonaisluvuksi.

## Katso myös:

1. Virallinen Elixir-dokumentointi: https://hexdocs.pm/elixir/Kernel.html#rand/0
2. Erlangin :rand:n dokumentointi: https://erlang.org/doc/man/rand.html
3. Erlangin :crypto:n dokumentointi: https://erlang.org/doc/man/crypto.html