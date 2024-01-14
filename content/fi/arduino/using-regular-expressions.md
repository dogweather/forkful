---
title:                "Arduino: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Arduino-ohjelmoinnissa?

Säännölliset lausekkeet ovat erittäin hyödyllinen työkalu Arduino-ohjelmoijille, jotka haluavat suorittaa monimutkaisia haku- ja korvaustoimintoja merkkijonoille. Ne säästävät aikaa ja vaivaa verrattuna perinteisiin merkkijonojen manipulointitapoihin.

## Miten käyttää säännöllisiä lausekkeita Arduino-ohjelmoinnissa?

Säännöllisten lausekkeiden käyttö Arduino-ohjelmoinnissa on helppoa. Ensimmäiseksi tarvitset RegularExpressions-kirjaston ja sen jälkeen voit käyttää säännöllisiä lausekkeita `Regex`-olion avulla. Tässä on esimerkki säännöllisten lausekkeiden käytöstä Arduino-ohjelmoinnissa:

```Arduino
#include <RegularExpressions.h>
// Luo uusi Regex-olio
Regex regex;
// Määritä haluttu säännöllinen lauseke merkkijonolle "Hello World"
regex.pattern("Hello (World)");
// Tutki merkkijonoa ja tulosta löydetyt tulokset sarjaporttiin
Serial.println(regex.match("Hello World").begin());
```

Yllä oleva koodi tulostaa sarjaporttikonsoliin löydetyn tekstin, eli "World". Säännöllisiä lausekkeita voi myös käyttää merkkijonojen korvaamisessa `regex.replace()`-funktion avulla.

## Syvemmälle säännöllisten lausekkeiden käyttöön

Säännölliset lausekkeet sisältävät erilaisia metakaraktereja ja ilmauksia, jotka mahdollistavat tarkemman haun ja korvauksen merkkijonoille. Esimerkiksi `*`-merkki merkitsee, että edellistä merkkiä voi olla useita kappaletta. `+`-merkki taas vaatii, että edellinen merkki esiintyy vähintään kerran. `.`-merkki vastaa yhtä merkkiä paitsi rivinvaihtoa.

Säännöllisten lausekkeiden käyttöön syvemmin tutustumalla voit tehdä monimutkaisempia haku- ja korvaustoimintoja Arduino-ohjelmissa.

## Katso myös
- [Regex-tunnuksien käyttö Arduino IDE:ssä](https://knowledge.ni.com/KnowledgeArticleDetails?id=kA00Z000001DbLtSAK&l=fi-FI)
- [Ohjeita säännöllisten lausekkeiden käyttöön Arduinon kirjastolla](https://github.com/nickgammon/arduino_sketches/blob/master/Regex/README.md)
- [Säännöllisten lausekkeiden cheat sheet](https://www.debuggex.com/cheatsheet/regex/javascript)