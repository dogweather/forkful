---
title:                "Elixir: Sanojen yhdistäminen"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Yhdistäminen nykyisiä merkkijonoja on yksi yleisimmistä toiminnoista ohjelmistokehityksessä. Se voi olla hyödyllistä, kun haluat luoda uuden merkkijonon eri osista tai vain yhdistää olemassa olevia merkkijonoja. Elixirin tarjoama yhdistämisfunktio tekee tästä prosessista helpompaa ja nopeampaa. 

## Miten tehdä

Yhdistäminen merkkijonoja Elixirissä on yksinkertaista ja suoraviivaista. Tässä on muutama esimerkki koodin kera, jotka näyttävät, kuinka se tapahtuu:

```Elixir
# Yksinkertainen yhdistäminen tekstistä

"This is " <> "a string." 

#=> "This is a string."

# Yhdistä merkkijonot numeroilla

"Number " <> " " <> 10

#=> "Number 10"

# Yhdistä dynaamiset merkkijonot käyttäen muuttujia

name = "Jussi"

"Hello " <> name <> "!"

#=> "Hello Jussi!"
```

Yllä olevat esimerkit käyttävät `<>` -operaattoria yhdistämään merkkijonoja. Tämä operaattori tekee saman kuin `+` yhdistäessä numeroita, mutta sen avulla voit yhdistää merkkijonoja.

## Syvällinen sukellus

Yhdistämisen lisäksi Elixirissä on myös muita tapoja käsitellä merkkijonoja. Yksi hyödyllisimmistä on `IO.puts` -funktio, joka tulostaa merkkijonon terminaaliin:

```Elixir
IO.puts("Merkkijonojen yhdistäminen")
#=> Merkkijonojen yhdistäminen
```

Voit myös käyttää `String.length` -funktiota selvittääksesi merkkijonon pituuden. Tämä on hyödyllistä esimerkiksi, jos haluat tietää, onko merkkijono tarpeeksi pitkä ennen sen yhdistämistä toiseen:

```Elixir
string = "Merkkijonojen pituus"

String.length(string)
#=> 20
```

Lisäksi voit käyttää `String.upcase` ja `String.downcase` muuttaaaksesi merkkijonon kirjainkoon, `String.reverse` kääntääksesi merkkijonon sekä muita hyödyllisiä funktioita.

## Katso myös

- [Elixirin dokumentaatio merkkijonojen yhdistämisestä](https://hexdocs.pm/elixir/String.html#concatenation/2)
- [Merkkijonotoiminnot Guilassa](https://www.guila.fi/opintokokonaisuus/ohjelmoinnin-tyokalut-johdatus-kieleen/)
- [Ohjeita merkkijonojen käsittelyyn Elixirissä](https://stackoverflow.com/questions/53376959/string-manipulation-in-elixir)

Kiitos lukemisesta! Toivottavasti tämä opas auttoi sinua oppimaan lisää merkkijonojen yhdistämisestä Elixirissä. Muista kokeilla erilaisia ​​toimintoja ja löytää parhaiten sopiva tapa käsitellä merkkijonoja ohjelmassasi.