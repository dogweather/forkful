---
title:                "Alimerkkijonojen erottelu"
html_title:           "Bash: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Substringien erottelu tarkoittaa tietyn osan erottamista merkkijonosta. Ohjelmoijat tekevät tätä esimerkiksi tarvittavan tiedon noutamiseksi suuresta merkkijonosta.

## Kuinka:
Kirjoita merkkijono, josta haluat erottaa alimerkkijonon, seuraavalla tavalla:
```
Bash substring="${string:start:length}"
```
Jossa "substring" on nimi uudelle alimerkkijonolle, "string" on alkuperäinen merkkijono, "start" on kohta, josta haluat aloittaa erottelun ja "length" on erottelun pituus.
Esimerkki käytöstä:
```
Bash string="Tämä on esimerkki" substring="${string:0:4}"
```
Tämä tulisi antaa tulosteena "Tämä".

## Syvemmälle:
Substringien erottelu on ollut käytössä jo pitkään ja se löytyy monista kielistä, mukaan lukien Bash. Joissakin muissa kielissä, kuten Java, substringit merkitään eri tavalla. Myös erottelun tekeminen sijoittamatta sitä uudelle merkkijonolle on mahdollista Bashissa. Voit lukea lisätietoja dokumentaatiosta.

## Katso myös:
https://www.gnu.org/software/bash/
https://tldp.org/LDP/abs/html/string-manipulation.html