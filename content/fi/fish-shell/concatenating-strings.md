---
title:                "Fish Shell: Jonojen yhdistäminen"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat yhdistää merkkijonoja koodissasi? Yhdistämällä merkkijonoja voit luoda dynaamisia ja muokattavia tekstejä, mikä on erityisen hyödyllistä esimerkiksi käyttäjien syötteiden käsittelyssä tai käyttöliittymän muokkaamisessa.

## Miten

```Fish Shell``` tarjoaa monia eri tapoja yhdistää merkkijonoja. Seuraavassa esimerkissä käytämme ```echo```-komennon yhteydessä ```&```-operaattoria yhdistämään kaksi stringiä:

```
echo "Tervetuloa " & "Finnish Reader"
```

Tämän tuloksena saamme:

```
Tervetuloa Finnish Reader
```

Toinen tapa yhdistää merkkijonoja on käyttää ```string replace``` -komentoa. Tässä esimerkissä korvaamme ensimmäisen merkkijonon toisella merkkijonolla:

```
set stringi "Fish Shell on paras"
string replace "paras" "mahtava" $stringi
```

Tulokseksi saamme:

```
Fish Shell on mahtava
```

## Syvempi sukellus

Fish Shellin ```string```-moduuli tarjoaa myös muita käteviä työkaluja merkkijonojen käsittelyyn, kuten ```split```, ```trim```, ```length``` ja ```reverse```. Voit tutustua näihin tarkemmin esimerkiksi Fish Shellin dokumentaatiosta.

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell - opas aloittelijoille](https://www.freecodecamp.org/news/the-beginners-guide-to-fish-shell-c37c3a7c726/)
- [Fish Shell - merkkijonojen yhdistäminen](https://www.linux.com/training-tutorials/learn-to-combine-strings-using-fish-shell/)