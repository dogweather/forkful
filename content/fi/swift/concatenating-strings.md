---
title:                "Swift: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Monissa Swift-sovelluksissa on tarvetta yhdistää merkkijonoja, jotta luodaan uusia, tarkoituksenmukaisia merkkijonoja. Tämä voi olla hyödyllistä esimerkiksi, jos halutaan näyttää käyttäjälle tietoja eri muodoissa yhden merkkijonon sijaan.

## Kuinka tehdä?

```Swift
// Alustetaan kaksi merkkijonoa
let nimi = "Maaria"
let tervehdys = "Hei "

// Yhdistetään merkkijonot
let lopputulos = tervehdys + nimi

// Tulostetaan lopputulos
print(lopputulos)

// Output: Hei Maaria
```

Merkkijonojen yhdistäminen voidaan tehdä käyttämällä plus-merkkiä (+), kuten yllä olevassa esimerkissä. Voit myös käyttää yhdistämiseen Swiftin tarjoamaa helpompaa tapaa, jossa käytetään '=' merkkiä, kuten alla olevassa esimerkissä:

```Swift
// Alustetaan kaksi merkkijonoa
let etunimi = "Juha"
let sukunimi = "Mäkinen"

// Yhdistetään merkkijonot
let kokonimi = etunimi += sukunimi

// Tulostetaan lopputulos
print(kokonimi)

// Output: JuhaMäkinen
```

## Syvällisemmin

Vaikka merkkijonojen yhdistäminen tuntuu helpolta ja suoraviivaiselta, on hyvä ymmärtää miten tämä tapahtuu taustalla. Swiftillä on tapana muuttaa merkkijonat ensin niin kutsutuiksi merkkijonoliteraaleiksi ja sitten yhdistää ne toisiinsa.

Lisäksi Swiftin merkkijonon yhdistäminen toimii hyvin myös muun tyyppisten arvojen kanssa, kuten kokonaislukujen ja desimaalilukujen. Tämä on hyödyllistä, jos haluat esimerkiksi näyttää käyttäjälle laskutoimitusten tuloksia selkeästi.

## Katso myös

- Swiftin virallinen dokumentaatio merkkijonojen yhdistämisestä: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID301
- Vinkkejä ja esimerkkejä merkkijonojen yhdistämisestä Stack Overflow -sivustolla: https://stackoverflow.com/questions/24467614/how-do-i-concatenate-strings-in-swift