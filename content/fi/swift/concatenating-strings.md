---
title:                "Swift: Merkkijonojen yhdistäminen"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja Swiftissä? Joissain ohjelmointitehtävissä saattaa olla tarpeen yhdistää useampi merkkijono yhdeksi, jolloin concatenaatio eli merkkijonojen yhdistäminen tulee tarpeeseen.

## Kuinka tehdä

Yhdistämistä varten Swiftissä on erityinen ```+``` operaattori, joka mahdollistaa kahden merkkijonon yhdistämisen yhdeksi. Katsotaanpa esimerkkiä:

```Swift
let etunimi = "Matti"
let sukunimi = "Meikäläinen"

let nimi = etunimi + " " + sukunimi

print(nimi) //tulostaa "Matti Meikäläinen"
```

Tässä esimerkissä yhdistimme ensin kaksi muuttujaa ```etunimi``` ja ```sukunimi``` yhdeksi nimeksi ```nimi```. Huomaa, että yhdistäessä merkkijonoja tulee välissä olla välilyöntiä, jotta nimet eivät ole kiinni toisissaan.

Voit myös yhdistää useampia kuin kaksi merkkijonoa kerrallaan:

```Swift
let aloituslause = "Hei, nimeni on"
let etunimi = "Matti"
let sukunimi = "Meikäläinen"
let loppulause = "ja olen Swift-kehittäjä."

let esittely = aloituslause + " " + etunimi + " " + sukunimi + ", " + loppulause

print(esittely) //tulostaa "Hei, nimeni on Matti Meikäläinen ja olen Swift-kehittäjä."
```

Huomaa, että voit myös yhdistää merkkijonoja ja muuttujia, kuten esimerkissä yhdistimme ```aloituslauseen```, ```etunimen```, ```sukunimen``` ja ```loppulauseen``` yhdeksi esittelyksi.

## Syvemmälle

Vaikka yhdistäminen merkkijonoja Swiftissä on helppoa ja kätevää, on hyvä muistaa, että se luo joka kerta uuden merkkijonon, mikä voi aiheuttaa suorituskyvyn hidastumista erityisesti suurilla merkkijonoilla.

On myös otettava huomioon, että yhdistäminen voi aiheuttaa virheitä, jos merkkijonassa on erikoismerkkejä tai numerotekstiä. Tällöin kannattaa käyttää `String(describing:)` tai `String(stringInterpolationSegment:)` -metodia, joka muuntaa muut tyypit merkkijonoksi ennen yhdistämistä.

## Katso myös

- [Swiftin perustoimintojen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Codecademyn opetusvideo Swiftin concatenaatiosta](https://www.youtube.com/watch?v=Wko-PkvR11Q)