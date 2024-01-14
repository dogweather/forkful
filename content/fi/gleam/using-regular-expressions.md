---
title:                "Gleam: Säännöllisten lausekkeiden käyttö"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

### Miksi käyttää säännöllisiä lausekkeita Gleam-ohjelmoinnissa?

Säännölliset lausekkeet ovat tehokas työkalu tekstien käsittelyyn ja haun suorittamiseen Gleam-ohjelmoinnissa. Niiden avulla voimme löytää ja muokata tiettyjä merkkijonoja nopeasti ja helposti. Ne ovat erityisen hyödyllisiä, kun käsittelemme suuria määriä dataa ja meidän täytyy suorittaa monimutkaisia hakuja tai muokkauksia.

### Kuinka käyttää säännöllisiä lausekkeita Gleam-ohjelmoinnissa?

Gleam tarjoaa sisäänrakennetun moduulin nimeltä `Regex`, joka sisältää kaikki tarvittavat toiminnot säännöllisten lausekkeiden käyttämiseen. Ensimmäiseksi meidän täytyy tuoda tämä moduuli tiedostoomme.

```
import Regex
```

Säännöllisen lausekkeen luomiseksi käytämme `Regex.make`-funktiota ja annamme sille kaksi parametria: säännöllisen lausekkeen ja sen valitsimen. Valitsin määrittää, kuinka laajasti säännöllinen lauseke otetaan huomioon haussa. Esimerkiksi jos käytämme valitsinta `global`, haku tehdään kaikilta merkkijonoilta, ei vain ensimmäiseltä.

```
let pattern = Regex.make("gleam", Regex.global)
```

Seuraavaksi voimme käyttää `Regex.replace`-funktiota muuttaaksemme säännöllisen lausekkeen osumat joksikin toiseksi merkkijonoksi. Voimme myös käyttää `Regex.match`-funktiota tarkistaaksemme, löytyykö säännöllinen lauseke annetusta merkkijonosta.

```
let result = Regex.replace(pattern, "Gleam is the best programming language!", "Elixir")
let match = Regex.match(pattern, "I love Gleam programming language!")
```

### Syvällinen tutustuminen säännöllisten lausekkeiden käyttöön Gleam-ohjelmoinnissa

Säännöllisten lausekkeiden käyttö Gleamissa on samanlaista kuin muissakin ohjelmointikielissä, mutta Gleam tarjoaa turvallisuutta ja luettavuutta parantavia ominaisuuksia. Ensinnäkin, kaikki säännölliset lausekkeet tarkistetaan Gleamissa käännösaikana, joten mahdolliset virheet havaitaan aikaisin. Lisäksi Gleamissa on vahva tyyppijärjestelmä, joka auttaa meitä varmistamaan, että käytämme oikeita parametreja ja että tulokset ovat oikean tyyppisiä.

Toiseksi, Gleamissa säännölliset lausekkeet käsitellään muuttumattomina tietorakenteina, mikä tarkoittaa, että muokatessamme säännöllistä lauseketta se palauttaa uuden säännöllisen lausekkeen sen sijaan, että muokkaisi alkuperäistä. Tämä on tärkeää, koska meidän täytyy olla tarkkoja siitä, ettei alkuperäistä dataa muuteta vahingossa.

### Katso myös

- [Gleam-kielen virallinen verkkosivusto](https://gleam.run/)
- [Gleam-kielen dokumentaatio](https://gleam.run/documentation)
- [Regex-moduulin dokumentaatio](https://gleam.run/modules/regex/latest/)

Kiitos lukemisesta ja onnea säännöllisten lausekkeiden käyttöön Gleam-ohjelmoinnissa!