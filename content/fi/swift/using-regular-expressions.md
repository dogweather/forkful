---
title:    "Swift: Säännöllisten lausekkeiden käyttö"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Swift-ohjelmoinnissa?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja tietojen käsittelyyn ja tiedon hakemiseen Swift-ohjelmoinnissa. Ne mahdollistavat monimutkaisten haku- ja korvaustoimintojen suorittamisen tekstissä. Niiden avulla voidaan tarkistaa, että tiettyä muotoa noudattavia tietoja käsitellään oikein ja tehokkaasti. 

## Kuinka käyttää säännöllisiä lausekkeita Swift-ohjelmoinnissa?

Säännöllinen lauseke koostuu merkeistä ja säännöistä, jotka määrittävät tekstiä, jota halutaan hakea ja korvata. Tässä esimerkissä käytämme säännöllistä lauseketta tarkistaaksemme, onko annetussa merkkijonossa numeroita:

```Swift
let input = "Swift on upea ohjelmointikieli, jota on helppo oppia!"
let regex = "[0-9]+" // säännöllinen lauseke
let matches = input.matches(regex: regex)
print(matches) // []
```

Ensimmäisessä rivissä luomme merkkijonon, jossa ei ole numeroita. Toisessa rivissä määritämme säännöllisen lausekkeen, joka etsii merkkijonossa olevia numeroita. Kolmannessa rivissä suoritamme haun käyttämällä `matches` -funktiota, joka palauttaa listan lausekkeen täyttävistä osista. Viimeisessä rivissä tulostamme tyhjän listan, koska merkkijonossa ei ollut numeroita.

Seuraavassa esimerkissä käytämme säännöllistä lauseketta korvaamaan merkkijonossa olevat välilyönnit alaviivoilla:

```Swift
let input = "Kirjoitetaan_yksi_sana"
let regex = "\\s+" // säännöllinen lauseke välilyöntejä varten
let result = input.replacingOccurrences(of: regex, with: "_")
print(result) // "Kirjoitetaan_yksi_sana"
```

Ensimmäisessä rivissä luomme merkkijonon, jossa on välilyöntejä. Toisessa rivissä määritämme säännöllisen lausekkeen, joka vastaa välilyöntejä. Kolmannessa rivissä käytämme `replacingOccurrences` -funktiota, joka korvaa merkkijonossa löytyvät vastaavat osat annetulla merkkijonolla. Viimeisessä rivissä tulostamme muokatun merkkijonon, josta välilyönnit on korvattu alaviivoilla.

## Syväsukellus säännöllisten lausekkeiden käyttämiseen Swift-ohjelmoinnissa

Säännöllisten lausekkeiden käyttäminen Swift-ohjelmoinnissa voi olla hieman haastavaa aluksi, mutta niiden avulla voidaan suorittaa monimutkaisia ja tehokkaita haku- ja korvaustoimintoja. On tärkeää ymmärtää säännöllisten lausekkeiden syntaksi ja erilaiset merkit ja säännöt, joita voidaan käyttää. Tässä artikkelissa esitellyt esimerkit ovat vain pintaraapaisu säännöllisten lausekkeiden käytöstä Swiftissä. Suosittelemme tutustumaan tarkemmin aiheeseen ja kokeilemaan erilaisia säännöllisiä lausekkeita omassa koodissasi.

## Katso myös

- [Ohjeet säännöllisten lausekkeiden käytt