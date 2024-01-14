---
title:                "Fish Shell: Alimerkkijonojen erottaminen"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat erottaa alijonoja Fish Shell -ohjelmointikielen avulla? Yksinkertaisesti sanottuna, se on erittäin kätevä tapa työstää merkkijonoja ja pystyt käyttämään erilaisia tapoja manipuloida niitä.

## Miten

Fish Shellilla on helppo erottaa alijonoja. Se tehdään `string` komennolla, jossa annetaan haluttu merkkijono ja sen jälkeen väleinä yksittäisiä merkkejä tai merkkijonoja, jotka haluat erottaa alkuperäisestä merkkijonosta.

```Fish Shell
string="Tämä on esimerkki merkkijonosta"
echo $string[5,-1] # Tulostaa "on esimerkki merkkijonosta"
echo $string[1,3] # Tulostaa "Täm"
```

Substrnign erottamista voidaan käyttää myös esimerkiksi muuttamaan olemassa olevaa merkkijonoa. Alla olevassa esimerkissä poistetaan välilyönnit merkkijonon alusta ja lopusta käyttäen `sub` komentoa.

```Fish Shell
string=" Hei, tämä on teksti "
echo $string # Tulostaa "Hei, tämä on teksti"
sub "^[ ]*" "" $string; string=$REPLY
sub "[ ]*$" "" $string; string=$REPLY
echo $string # Tulostaa "Hei, tämä on teksti"
```

## Syvällinen sukellus

Fish Shellin `string` komento tukee myös monia muita hyödyllisiä toimintoja, kuten merkkijonon jakamista eri osiin käyttäen esimerkiksi `cut` tai `split` komentoja. Lisäksi voit käyttää regular expressioneita erottamaan alijonoja haluamallasi tavalla.

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Fish Shellin syvällisempi opas](https://fishshell.com/docs/current/tutorial.html)
- [Merkkistringien muokkaaminen Fish Shellillä](https://medium.com/noob-programming/%C3%A4lykk%C3%A4%C3%A4sti-merkkijonojen-k%C3%A4sittely-ohjelmallasi-fish-shellill%C3%A4-sa1i-o1k-a1e66c6abad0)