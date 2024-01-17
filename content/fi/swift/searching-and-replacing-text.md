---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Swift: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä se on ja miksi?
Etsiminen ja tekstin korvaaminen on tärkeä osa ohjelmointia. Kun koodaamme, me käsittelemme suurta määrää tekstiä, joka voi olla virheellistä tai muuttua usein. Joskus meidän täytyy etsiä tiettyjä sanoja tai lausekkeita ja korvata ne toisilla. Tämä auttaa meitä vähentämään virheitä ja tekee koodin muokkauksen helpommaksi.

## Kuinka tehdä se:
Voit etsiä ja korvata tekstiä Swiftissä käyttäen `replacingOccurrences(of:with:)` -metodia. Tämä metodi ottaa ensimmäisenä parametrina etsittävän tekstin ja toisena parametrina korvaavan tekstin. Esimerkiksi, jos haluat korvata kaikki "hello" -sanat "hei" -sanalla, käyttäisit seuraavaa koodia:
```Swift
let teksti = "Tervetuloa maailmaan!"
let muokattuTeksti = teksti.replacingOccurrences(of: "Tervetuloa", with: "Hei")
print(muokattuTeksti) // tulostaa "Hei maailmaan!"
```
Voit myös käyttää `replacingOccurrences(of:with:options:)` -metodia, joka antaa sinulle enemmän vaihtoehtoja, kuten isot ja pienet kirjaimet huomioiminen. Voit lukea lisää näistä vaihtoehdoista [Swiftin dokumentaatiosta](https://developer.apple.com/documentation/foundation/nsstring/1410083-replacingoccurrences).

## Syvemmälle:
Etsiminen ja korvaaminen on ollut osa ohjelmointia jo kauan ennen Swiftiä. Nykyään on olemassa myös muita tapoja etsiä ja korvata tekstiä, kuten käyttämällä säännöllisiä lausekkeita. Joissakin tapauksissa voit myös käyttää `replaceSubrange(_:with:)` -metodia, joka korvaa tietyn osan merkkijonosta toisella osalla.

## Katso myös:
Swiftin [virallinen dokumentaatio](https://developer.apple.com/documentation/swift/string) sisältää lisätietoa tekstin etsimisestä ja korvaamisesta. Voit myös tutustua [säännöllisiin lausekkeisiin Swiftissä](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift).