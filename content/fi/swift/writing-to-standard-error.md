---
title:                "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
html_title:           "Swift: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
simple_title:         "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Kirjoittaminen standardivirheeseen on kätevä tapa ohjelman suoritusvirheiden hallintaan. Kun ohjelmassa tapahtuu virhe, se kirjoitetaan standardivirheeseen, jolloin se näkyy suoraan ohjelman suorituksen seurannassa. Tämä helpottaa virheen havaitsemista ja korjaamista.

# Miten?
### Swift:ssä standardivirheeseen kirjoittaminen tapahtuu käyttämällä print-funktiota ja säädetään parametriksi .standardError.
```Swift
print("Tämä on virheviesti.", to: .standardError)
```
Tämän jälkeen viesti näkyy ohjelman suorituksen seurannassa punaisella värillä, jolloin se erottuu muusta tekstistä.

### Esimerkki:
```Swift
let luku1 = 10
let luku2 = 0

if luku2 == 0 {
    print("Toinen luvuista ei voi olla nolla.", to: .standardError)
} else {
    let tulos = luku1 / luku2
    print("Jakolaskun tulos on \(tulos).")
}
```
Tässä esimerkissä tulee virhe, jos luku2 on nolla. Ohjelma tulostaa tällöin virheviestin standardivirheeseen ja pysähtyy. Muussa tapauksessa ohjelma tulostaa normaalisti jakolaskun tuloksen.

# Syvempi sukellus
### Historiallinen konteksti:
Standardivirheeseen kirjoittaminen on ollut käytössä Unix-käyttöjärjestelmissä jo vuosikymmeniä. Sen avulla on mahdollista kontrolloida ja ohjata ohjelmien virhetilanteita.

### Vaihtoehtoja:
Pythonissa ja C:ssä käytetään vastaavia toimintoja, kuten sys.stderr ja perror().

### Toteutus:
Swift:ssä standardivirheen tulostaminen onnistuu käyttämällä Foundation-frameworkia, joka tarjoaa print-funktiolle .standardError-parametrin.

# Katso myös:
- [Foundation - Apple Developer Documentation](https://developer.apple.com/documentation/foundation)
- [Swift Standard Library - Apple Developer Documentation](https://developer.apple.com/documentation/swift)