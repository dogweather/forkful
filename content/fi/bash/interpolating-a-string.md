---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Merkkijonon interpolointi tarkoittaa muuttujan sijoittamista suoraan merkkijonoon. Koodarit tekevät tämän työn tehokkuuden ja luettavuuden parantamiseksi.

## Kuinka:

Seuraavassa on esimerkki siitä, kuinka Bashissa merkkijonon interpolointi tehdään:

```Bash
nimi="Linus"
echo "Hei, olen $nimi"
```

Tämä tulostaa:

```Bash
Hei, olen Linus
```

Voit myös käyttää kierteitä arpakuutioiden heittämisessä:

```Bash
arpa=$((RANDOM % 6 + 1))
echo "Arpakuutiosi numero on $arpa"
```

Tämä voi tuottaa esimerkiksi:

```Bash
Arpakuutiosi numero on 4
```

## Deep Dive

Merkkijonon interpolointi on ollut osa Unix shell skriptausta sen varhaisista päivistä lähtien ja on levinnyt muihin kieliiin. Sen vastineena, voit käyttää `printf`-toimintoa, joka on peräisin C-kielestä. Kuten Pythonissa, Bashissa myös merkkijonon interpoloinnin voi tehdä useammalla tavoin, kuten "Halo ${nimi}, mitä kuuluu?".

## See Also

Merkkijonon käsittelystä saadaksesi lisätietoa, tutustu seuraaviin linkkeihin:

1. Bash-ohjelmointiopas: Merkkijonojen interpolointi: 
https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion

2. Stack Overflow: Kuinka interpoloida merkkijonoja Bashissa:
https://stackoverflow.com/questions/4181703/how-can-i-concatenate-string-variables-in-bash