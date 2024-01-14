---
title:                "Swift: Mallia vastaavien merkkien poistaminen"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi poistaa nimikkeitä mallin mukaan?

Nimikkeiden poistaminen, jotka vastaavat tiettyä mallia, voi olla hyödyllistä, jos haluat suodattaa tietyn tyyppisen sisällön tai löytää tietyntyyppisiä merkintöjä. Esimerkiksi, jos sinulla on tekstiä, jossa on sekä numeroita että kirjaimia ja haluat poistaa vain numerot, voit käyttää tätä toimintoa.

## Miten se tehdään?

Voit käyttää Swiftin `replacingOccurrences(of:with:options:)` -funktiota poistaaksesi merkkejä, jotka vastaavat haluamaasi mallia. Esimerkiksi jos haluat poistaa kaikki numerot merkkijonosta, voit käyttää seuraavaa koodia:

```Swift
let teksti = "123abc456"
let vainKirjaimet = teksti.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)

print(vainKirjaimet)

// Output: "abc"
```

Kuten näet, käytämme `[0-9]` mallia, joka vastaa kaikkia numeroita. Voit käyttää myös muita mallinpään tai säännöllisiä lausekkeita poistaaksesi tietyn tyyppisiä merkkejä.

## Syvällisempi tarkastelu

Swiftin `replacingOccurrences(of:with:options:)` -funktio toimii käyttämällä säännöllisiä lausekkeita. Tämä tarkoittaa, että voit käyttää monimutkaisempia malleja ja jopa luoda omia sääntöjä. Voit myös käyttää erilaisia ```options``` -arvoja määrittääksesi, miten poistaminen tapahtuu. Tarkempi kuvaus säännöllisistä lausekkeista ja `options` -arvoista löytyy [Swiftin dokumentaatiosta](https://developer.apple.com/documentation/swift/string/1786171-replacingoccurrences).

## Katso myös

- [Swiftin dokumentaatio säännöllisistä lausekkeista](https://developer.apple.com/documentation/swift/string/2878330).
- [NSHipsterin opas säännöllisiin lausekkeisiin Swiftissa](https://nshipster.com/swift-regular-expressions/).
- [Apple:n esimerkkejä säännöllisistä lausekkeista Swiftissä](https://developer.apple.com/library/archive/documentation/General/Conceptual/DevPedia-CocoaCore/RegularExpressions.html).