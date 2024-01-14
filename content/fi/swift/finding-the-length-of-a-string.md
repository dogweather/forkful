---
title:                "Swift: Merkkijonon pituuden löytäminen"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

On monia syitä, miksi haluat selvittää merkkijonon pituuden Swift-ohjelmoinnissa. Se voi olla tarpeen tarkistaa käyttäjän antaman syötteen oikeellisuus tai suorittaa tiettyjä operaatioita riippuen merkkijonon pituudesta. Riippumatta syystä, on tärkeää ymmärtää, miten tämä tehdään nopeasti ja tehokkaasti.

## Kuinka?

Merkkijonon pituuden löytäminen Swiftillä on hyvin yksinkertaista. Voit käyttää `count`-metodia merkkijonolle ja se palauttaa merkkien määrän. Esimerkiksi:

```Swift
let merkkijono = "Hei, maailma!"
print(merkkijono.count) // tulostaa 13
```

Joskus merkkijonon pituus saattaa olla tarpeen myös pyöräyttää silmukassa ja suorittaa tiettyjä toimintoja jokaiselle merkille. Tämä voidaan tehdä käyttämällä `characters`-ominaisuutta ja sen `count`-metodia. Esimerkiksi:

```Swift
let merkkijono = "Kissapöllö"
for merkki in merkkijono.characters {
    print("Merkki: \(merkki)")
}
// tulostaa: K I S S A P Ö L L Ö
```

## Syvempi katsaus

Vaikka merkkijonon pituuden löytäminen saattaa vaikuttaa yksinkertaiselta, se voi silti aiheuttaa ongelmia, jos et ymmärrä, miten Swift käsittelee merkkijonoja. Esimerkiksi, jos haluat löytää merkkijonon pituuden, jossa on erikoismerkkejä tai emoji-ikoneja, `count`-metodi ei välttämättä palauta odottamaasi vastausta. Tämä johtuu siitä, että jotkut merkkijonon merkit voivat sisältää useita Unicode-kerroksia, ja tämä vaikuttaa merkkien lukumäärään.

Voit kiertää tämän ongelman käyttämällä `unicodeScalars`-ominaisuutta ja sen `count`-metodia. Tämän avulla voit saada tarkan lukumäärän merkkien lukumäärästä huolimatta niiden sisältämistä Unicode-kerroksista.

```Swift
let merkkijono = "🐱😸"
print(merkkijono.count) // tulostaa 2
print(merkkijono.unicodeScalars.count) // tulostaa 4
```

## Katso myös

- [Swiftin merkkijonojen dokumentaatio](https://developer.apple.com/documentation/swift/string)
- [Lyhyt opas Swiftin merkkijonoihin](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html)