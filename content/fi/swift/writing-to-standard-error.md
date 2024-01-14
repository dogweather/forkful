---
title:                "Swift: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan tätä blogikirjoitusta kirjoittamisesta standardiin virheeseen Swift-ohjelmointikielellä. Tässä artikkelissa käymme läpi syitä siihen, miksi kirjoittaisit tietoa standardiin virheeseen ja miten se voisi hyödyttää sinua ohjelmoinnissa.

## Miten

```Swift
func divideNumbers(x: Int, y: Int) throws {
    guard y != 0 else {
        throw CustomError("Error: Y cannot be 0!")
    }
    
    let result = x / y
    print(result)
}

do {
    try divideNumbers(x: 10, y: 0)
} catch {
    print(error)
}
```

Koodinpätkässä esitetään yksinkertainen funktio, joka jakaa kaksi lukua ja heittää virheen, jos toinen numero on 0. Kun koodia ajetaan, virhe tulostetaan standardiin virheeseen, mikä auttaa sinua tunnistamaan, missä osassa koodia virhe tapahtui.

```
Error: Y cannot be 0!
```

Tämä esimerkki osoittaa, että standardiin virheen kirjoittaminen voi auttaa sinua vianetsinnässä ja virheiden tunnistamisessa koodissasi.

## Syvempää tietoa

Standardi virheeseen kirjoittaminen antaa sinulle mahdollisuuden hallita virheitäsi suoraan koodissasi. Tämä voi olla hyödyllistä erityisesti silloin, kun haluat näyttää käyttäjille tarkemman virheviestin kuin pelkän koodinvirheen.

Voit myös käyttää asettaa eri tasoisia virheitä, joista jotkut voidaan käsitellä ja jotkut jätetään ohjelmaa käyttävän kehittäjän vastuulle.

## Katso myös

Tässä on muutamia hyödyllisiä linkkejä, jotka voivat auttaa sinua oppimaan lisää tietoa standardiin virheeseen kirjoittamisesta:

- [Apple:n virallinen dokumentaatio Swift-ohjelmointikielestä](https://developer.apple.com/swift/)
- [Swift-yhteisön foorumit ja keskustelupalstat](https://swift.org/community/)
- [Ohjelmointikielen kehittäjien vinkkejä virheiden käsittelyyn](https://www.mokacoding.com/blog/throwing-error-swift/