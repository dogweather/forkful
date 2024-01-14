---
title:                "Bash: Regulareiden käyttö ohjelmoinnissa"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat voimakas työkalu, jota ohjelmoijat voivat hyödyntää tekstin käsittelyssä. Ne sallivat tarkan haun ja korvauksen tekstissä, mikä säästää aikaa ja vaivaa käsin tehdystä työstä.

## Miten käyttää säännöllisiä lausekkeita?

Säännöllisten lausekkeiden käyttö Bash-ohjelmoinnissa vaatii muutaman peruskäsitteen opettelua.

### Yksinkertaistaen

Säännöllinen lauseke (regular expression) on merkkijono, jota voidaan käyttää haun tekemiseen tai korvaamiseen tekstissä. Bashissa säännölliset lausekkeet asetetaan lainaavaaiden '`' merkkien väliin:

```Bash
grep 'hakusana' tiedosto.txt
```

### Varmistaminen

Joskus haluat selvittää, löytyykö tietyllä tavalla muotoiltua tekstiä tiedostosta tai muusta lähteestä. Voit tarkistaa tämän `grep`-komentolla ja säännöllisellä lausekkeella:

```Bash
grep '^[A-Z0-9]*' tiedosto.txt
```

Tämä haku tarkistaa, löytyykö tiedostosta sanoja, jotka alkavat joko isoilla kirjaimilla tai numerolla. `^` merkki merkitsee tiedoston alussa olevaa sanaa ja `*` merkki merkitsee yhden tai useamman merkin löytymistä.

### Selvittäminen

Toisinaan haluat korvata tiettyjä sanoja tai lausekkeita toisilla. Voit tehdä tämän `sed`-komentolla ja säännöllisellä lausekkeella:

```Bash
sed 's/vanha_sanastema/uusi_sanastotermi/g' tiedosto.txt
```

Tämä komento korvaa kaikki `vanha_sanastema` sanat tai lausekkeet `uusi_sanastotermi` sanoilla tiedostossa. `g` tarkoittaa, että korvaaminen tehdään koko tiedostossa.

## Syventävä tieto säännöllisistä lausekkeista

Säännölliset lausekkeet ovat paljon suurempi aihe kuin tässä voitaisiin affoida. Käydään kuitenkin alustavasti läpi muutama hyödyllinen käsite:

* `.` merkki tarkoittaa minkä tahansa yksittäisen merkin etsimistä
* `|` merkki tarkoittaa vaihtoehtoja (esim. `koira|kissa` etsii sanoja 'koira' tai 'kissa')
* `+` merkki tarkoittaa yhden tai useamman edellisen merkin etsimistä (esim. `a+` etsii yhden tai useamman a-kirjaimen)
* `( )` sulkeet auttavat ryhmittämään säännöllisiä lausekkeita

Tässä on muutama esimerkki säännöllisistä lausekkeista, jotka voisivat olla hyödyllisiä Bash-ohjelmoijille:

* `^[0-9]+$` tarkistaa, että sana on kokonaisluku
* `^[a-z]+@[a-z]+\.[a-z]+$` tarkistaa, että sana on sähköpostiosoite
* `^\([0-9]{3}\)-[0-9]{3}-[0-9]{4}$` tarkistaa, että sana on puhelinnumero muodossa (123)-456-7890

## Katso myös

* [Linuxin opas säännöllisille lausekkeille Bashissa](https