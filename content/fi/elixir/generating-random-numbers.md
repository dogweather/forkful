---
title:    "Elixir: Satunnaislukujen luominen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi Käyttää Elixirin Satunnaisten Numeroiden Luontia?

On monia erilaisia syitä, miksi voisi olla tarpeellista luoda satunnaisia numeroita Elixirillä. Jotkut esimerkkejä voivat olla simulaatiot, tietokannan testaus tai yksinkertaisesti satunnaisen luvun generointi erilaisiin ohjelmointiharjoituksiin. Satunnaisten numeroiden luominen on myös hauska tapa tutustua Elixirin ominaisuuksiin ja toimintoihin.

## Kuinka Tehdä Satunnaisia Numeroita Elixirillä?

Elixirillä satunnaisia numeroita voidaan luoda käyttämällä moduulia nimeltä `:rand`. Tämä moduuli sisältää useita funktioita, joilla voi luoda erilaisia satunnaisia numeroita.

### Satunnainen Kokonaisluku

Aloita importoimalla `:rand` moduuli:

```
import :rand
```

Voit sitten käyttää `rand/0` funktiota luomaan satunnaisen kokonaisluvun:

```
random_number = rand()
```

Seuraava koodi näyttää satunnaisen kokonaisluvun väliltä 1-100:

```
1..100 |> Enum.random() #=> 58
```

### Satunnainen Liukuluku

Satunnaisia liukulukuja varten, voit käyttää `float/1` funktiota ja antaa sille halutun desimaalien määrän:

```
random_float = float(3) #=> 0.278
```

### Satunnainen Merkkijono

Jos haluat generoida satunnaisen merkkijonon, voit käyttää `string/1` funktiota ja antaa sille halutun merkkien määrän:

```
random_string = string(10) #=> "jDUmvHWp9u"
```

## Syvällisempi Sukellus

Satunnaisia numeroita luodessa Elixir käyttää Mersenne Twister -algoritmia, joka on yksi tunnetuimmista ja nopeimmista satunnaislukugeneraattoreista. Tämä algoritmi hyödyntää lohkoketjua, joka mahdollistaa suuren määrän erilaisia satunnaisia numeroita.

Voit myös asettaa halutun alunumeron `rand/1` funktioon, joka mahdollistaa samojen satunnaisien numeroiden generoinnin uudelleen. Tämä on hyödyllistä esimerkiksi testimielessä.

## Katso Myös

- Elixirin virallinen dokumentaatio `:rand` moduulista: https://hexdocs.pm/elixir/1.12.1/Kernel.html#rand/0
- Mersenne Twister -algoritmin selitys: https://en.wikipedia.org/wiki/Mersenne_Twister