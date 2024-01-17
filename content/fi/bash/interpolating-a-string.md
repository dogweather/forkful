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

## Mitä ja miksi?
Interpolointi on tekniikka, jolla voimme lisätä muuttujan arvoja merkkijonoon Bash-skriptissä. Tämä on hyödyllistä esimerkiksi silloin, kun haluamme tulostaa dynaamisesti vaihtelevia tekstejä tai muuttujien arvoja. 

## Miten:
Käytämme merkintää ${} lisätäksemme muuttujan arvon merkkijonoon. Katso seuraava esimerkki:
```Bash
name="Mikko"
echo "Tervehdys, ${name}! Tänään on $(date +"%A")."
```
Tämä tulostaa: "Tervehdys, Mikko! Tänään on maanantai."

## Syvällisempää:
Interpolointi on ollut käytössä jo vuosikymmenten ajan. Ennen sitä käytettiin vain muista kielistä lainattuja funktioita, kuten sed tai awk, interpolointiin Bash-skripteissä. Joissakin muissa ohjelmointikielissä kutsutaan tätä myös muuttujien sisällyttämiseksi merkkijonoon.

## Katso myös:
https://ss64.com/bash/syntax-expand.html - Lisää tietoa interpoloinnista Bashissa.
https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion - Interpolointiin liittyvät Bash-dokumentaatiot.