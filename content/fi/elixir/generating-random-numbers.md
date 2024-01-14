---
title:                "Elixir: Satunnaisten numeroiden luominen"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmissa tarvitaan satunnaisia lukuja. Tämä voi olla esimerkiksi pelisovelluksessa, missä halutaan arpoa erilaisia tilanteita pelaajalle. Elixirin avulla tämä onnistuu helposti ja nopeasti.

## Miten

Random-moduuli tarjoaa valmiin toiminnon satunnaisien lukujen generoimiseksi. Se otetaan käyttöön yksinkertaisesti näin:

```Elixir
import Random

num = random(1..10)
IO.puts(num) # Tulostaa satunnaisen luvun väliltä 1-10
```

Voit myös määrittää tarkan lukumäärän satunnaisia numeroita haluamaltasi väliltä:

```Elixir
import Random

nums = Stream.repeatedly(fn() -> random(20..30) end) |> Enum.take(10)
IO.inspect(nums) # Tulostaa listan, jossa on 10 satunnaista lukua väliltä 20-30
```

## Syventävä tarkastelu

Satunnaislukujen generointi perustuu seediin, eli "alkulukuun". Seediä voi vaihtaa tarpeen mukaan, jolloin samaa tulosta ei saada joka kerta. Tämä on hyödyllistä esimerkiksi testauksessa, missä tarvitaan erilaisia satunnaisia lukuja.

Random-moduuli tarjoaa myös muita toimintoja, kuten shuffle- ja coin_flip-funktiot. Lisäksi voit generoida satunnaisia merkkijonoja valitsemaltaan merkkialueelta. Lisätietoja löytyy Elixirin virallisesta dokumentaatiosta.

## Katso myös

- [Elixirin virallinen dokumentaatio: Random](https://hexdocs.pm/elixir/Random.html)
- [Ohjelmointikielet: Elixir (suomeksi)](https://finnprogs.github.io/elixir-suomeksi/)
- [Kuinka käyttää Elixiria satunnaisien lukujen generoimiseen](https://www.keyandneedle.com/blog/random-numbers-using-elixir/)