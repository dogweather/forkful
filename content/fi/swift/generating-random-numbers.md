---
title:                "Swift: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi: Satunnaislukujen generoiminen

Satunnaislukujen generoiminen on hyödyllinen taito Swift-ohjelmoinnissa, sillä se mahdollistaa ohjelman erilaisten toimintojen satunnaisuuden simuloimisen ja monipuolistamisen.

## Kuinka: Esimerkkejä

```Swift
// Generoidaan yksi satunnainen kokonaisluku väliltä 1-100
let randomNumber = Int.random(in: 1...100)
print(randomNumber) // Tulostaa esim. 56

// Generoidaan satunnainen liukuluku väliltä 0.0 - 1.0
let randomFloat = Float.random(in: 0.0...1.0)
print(randomFloat) // Tulostaa esim. 0.78346

// Generoidaan satunnainen merkkijono kolmesta satunnaisesta kirjaimesta
let randomString = String((0..<3).map{ _ in Character(UnicodeScalar(Int.random(in: 65...90))!) })
print(randomString) // Tulostaa esim. "EJQ"

// Generoidaan satunnainen Bool-arvo
let randomBool = Bool.random()
print(randomBool) // Tulostaa joko "true" tai "false"
```

Näiden esimerkkien avulla voit helposti generoida satunnaisia arvoja omiin ohjelmiisi.

## Syvällisempi tarkastelu

Swiftin Foundation-kirjaston Random-luokassa on monia muitakin toimintoja satunnaislukujen generoimiseen. Voit esimerkiksi määrittää tietyn alkuluvun ja tulostaa sen kaikki jakajat tai generoida satunnaisen arvon annetun jakauman mukaan.

Kannattaa myös muistaa, että satunnaislukujen generoiminen voi olla hyödyllistä esimerkiksi testaamisessa ja pelien luomisessa.

## Katso myös

- [Apple Developer Documentation: Random](https://developer.apple.com/documentation/foundation/random)
- [Hacking with Swift: How to generate random numbers in Swift](https://www.hackingwithswift.com/example-code/system/how-to-generate-random-numbers-in-swift)