---
title:    "Swift: Alimerkkien erottelu"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Substringien eristäminen on tärkeä taito joka Swift-ohjelmoijan tulisi hallita. Se mahdollistaa merkkijonojen osien hakemisen ja manipuloinnin erityisen kätevästi. Tässä blogipostauksessa opimme kuinka helposti voit käyttää substringien eristämistä ohjelmassasi.

## Kuinka

Aloitamme yksinkertaisesti: avataan uusi Xcode-projekti ja lisätään seuraava koodi ```Swift
let lause = "Hei kaikki!"

let alkuosa = lause.prefix(4)
print(alkuosa) // tulostaa "Hei "
```

Yllä olevassa esimerkissä käytämme `prefix`-funktiota joka eristää merkkijonon ensimmäiset neljä merkkiä. Voimme myös käyttää `suffix`-funktiota eristämään merkkijonon viimeiset merkit. Voit mukauttaa näitä funktioita muuttamalla niiden parametreja.

Jos haluat eristää tietyn osan merkkijonosta, voit käyttää `range`-toimintoa sekä `substring`-funktiota seuraavalla tavalla: ```Swift
let numerot = "0123456789"

let osa = numerot[numerot.index(numerot.startIndex, offsetBy: 3)..<numerot.index(numerot.startIndex, offsetBy: 6)]
print(osa) // tulostaa "345"
```

Yllä olevassa esimerkissä käytämme `index`-funktiota löytääksemme halutun osan merkkijonosta. Huomaa myös, että merkkijono alkaa indeksistä 0.

## Syventävä tarkastelu

Nyt kun olet oppinut kuinka helposti voit käyttää substringien eristämistä ohjelmassasi, on hyvä tietää joitain lisätietoja. Substringit ovat viittauksia alkuperäiseen merkkijonoon eivätkä ne vaadi uuden merkkijonon luomista, mikä tekee prosessista tehokkaan. Lisäksi, jos muutat substringiä, se muuttaa myös alkuperäistä merkkijonoa.

On myös tärkeää muistaa, että substringit ovat aina `[String.SubSequence]`-tyyppejä, joten jos haluat käyttää niitä esimerkiksi `UILabel`-näyttöihin, sinun täytyy ensin muuntaa niitä `String`-muotoon.

## Katso myös

Tässä muutamia hyödyllisiä resursseja joiden avulla voit oppia lisää substringien eristämisestä Swiftissä:

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID288)
- [Hacking with Swift: Merkkijonojen käsittely](https://www.hackingwithswift.com/quick-start/understanding-swift/strings-in-swift)
- [Swift-merkkijonoviestintä: pienimmät tapaukset](https://www.swiftbysundell.com/articles/string-interpolation-in-swift/)