---
title:                "Swift: Virheenkorjaustulosteen tulostaminen"
simple_title:         "Virheenkorjaustulosteen tulostaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen koodari tietää, että debug-kehitys on oleellinen osa ohjelmointia. Mutta miksi tulostaisimme debug-tietoa? Se on hyvä tapa tarkistaa, mitä ohjelma on tekemässä ja löytää mahdolliset virheet.

## Kuinka tehdä

Tulostaessa debug-tietoa Swift-ohjelmassasi, sinun tulee käyttää print()-funktiota. Alla on muutama esimerkki, miten voit tulostaa erilaisia tietotyyppejä:

```Swift
let name = "Matti"
print("Hei "+name+", tervetuloa!")

let age = 25
print("Olet "+String(age)+" vuotta vanha.")
```

Tulostaa seuraavan:

```
Hei Matti, tervetuloa!
Olet 25 vuotta vanha.
```

Voit myös tulostaa muuttujien arvot array- ja dictionary-tyyppisille muuttujille:

```Swift
let fruits = ["omena", "banaani", "appelsiini"]
print("Suosikkifruittisi ovat:")
for fruit in fruits {
    print("- "+fruit)
}

let person = ["nimi": "Emma", "ikä": 30]
print("Hän on "+person["name"]+" ja hän on "+String(person["age"])+" vuotta vanha.")
```

Tulostaa seuraavan:

```
Suosikkifruittisi ovat:
- omena
- banaani
- appelsiini
Hän on Emma ja hän on 30 vuotta vanha.
```

Voit myös tulostaa erilaisia tietotyyppejä yhdessä, esimerkiksi:

```Swift
let isTall = true
let height = 180.5
print("Oletko pitkä? "+String(isTall))
print("Kuinka pitkä olet? "+String(height)+" cm.")
```

Tulostaa seuraavan:

```
Oletko pitkä? true
Kuinka pitkä olet? 180.5 cm.
```

## Syvempi sukellus

Print() toiminto hyödyllinen debug-tulostuksessa, mutta sitä ei tulisi käyttää lopullisessa koodissa. Sen sijaan voit käyttää esimerkiksi debuggeria tai logeja.

On myös hyvä huomata, että tulostettavat tiedot voivat aiheuttaa tietoturvariskejä, mikäli sovellus käsittelee arkaluonteista tietoa. Siksi on tärkeää olla varovainen, mitä tietoja tulostaa konsoliin.

## Katso myös

- [Swiftin virallinen dokumentaatio print()-funktiosta](https://developer.apple.com/documentation/swift/1541053-print)
- [Debug-tulostusten parhaat käytännöt Swiftissä](https://medium.com/@financiermathieu/best-practices-for-debugging-in-swift-4289d5d4d8e5)