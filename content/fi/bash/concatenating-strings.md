---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Bash: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Stringien yhdistäminen on silloin, kun yhdistät useita tekstipätkiä yhdeksi kokonaisuudeksi. Tämä on yleinen ohjelmoinnin tehtävä, koska se antaa meille mahdollisuuden luoda dynaamisia ja muuttuvia viestejä tai tiedostoja.

## Miten:
Bash-kielellä meillä on mahdollisuus helposti yhdistää merkkijonoja yhdessä komennolla "concat" tai käyttämällä "+" operaattoria.

```Bash
sana1="Tervetuloa"
sana2="Finnish"
kokonainen_lause=$sana1$sana2
echo $kokonainen_lause
```

Tulostaa: "TervetuloaFinnish".

## Deep Dive:
Stringien yhdistäminen on ollut osa koodausta jo melko pitkään. Alunperin se tehtiin useilla koodiriveillä, mutta nykyään monet kielet, kuten Bash, tarjoavat valmiita toimintoja tähän tehtävään. On myös olemassa muita tapoja yhdistää merkkijonoja, kuten käyttämällä formatointitoimintoja tai liittämällä merkkijonot yhteen eri tavoin.

## Katso myös:
- [Bash-kongurointi](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- [Stringien manipulointi Bash:ssa](https://ryanstutorials.net/bash-scripting-tutorial/bash-string-manipulation.php)