---
title:    "Swift: Kuviota vastaavien merkkien poistaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Se voi olla hyödyllistä esimerkiksi silloin, kun käsitellään tekstiä tai merkkijonoja ja halutaan poistaa tiettyä tyyppisiä merkkejä.

## Kuinka

Seuraavassa esimerkissä näytämme, miten voit poistaa kaikki välilyönnit, numerot ja erikoismerkit annetusta merkkijonosta.

```Swift
//Luodaan aluksi merkkijono, josta haluamme poistaa merkkejä
let teksti = "Hei! Tässä on esimerkkilause 1234."
//Luodaan kaava, joka määrittelee minkä tyyppiset merkit haluamme poistaa
let kaava = CharacterSet.alphanumerics.inverted
//Suoritetaan poisto käyttäen kaavaa ja tallennetaan uuteen muuttujaan
let uusiTeksti = teksti.components(separatedBy: kaava).joined()
//Tulostetaan uusi muuttuja, jossa poistetut merkit eivät enää näy
print(uusiTeksti)
//Tulos: "HeiTässäonesimerkkilause"
```

## Syvempää tietoa

Yllä oleva esimerkki käytti CharacterSet-luokkaa, joka tarjoaa valmiita kaavoja eri merkkityyppien poistamiseen. Voit myös luoda omia kaavoja, esimerkiksi poistamaan tiettyjä erikoismerkkejä tai merkkijonoja. Voit myös käyttää for-silmukoita poistaaksesi tiettyjä merkkejä merkkijonosta.

## Katso myös

- [String - Swift Documentation](https://developer.apple.com/documentation/swift/string)
- [CharacterSet - Swift Documentation](https://developer.apple.com/documentation/foundation/characterset)
- [How to Remove Characters from String in Swift?](https://www.appcoda.com/swift-chars/)