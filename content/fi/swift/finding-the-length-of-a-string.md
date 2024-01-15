---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Swift: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko saada selville merkkijonon pituuden? Lue tämä artikkeli ja opi miten se tapahtuu Swiftin avulla!

## Miten

Merkkijonon pituuden saaminen Swiftissä on helppoa. Sinun tarvitsee vain käyttää ".count" -metodia merkkijonon perään. Tämä metodi laskee merkkijonon kaikki merkit ja antaa lukumäärän takaisin.

```Swift
let string = "Tämä on merkkijono"
print(string.count)

// Output: 20
```

Tämä metodi toimii myös emoji-merkkien kanssa, sillä jokainen merkki lasketaan yhdeksi.

```Swift
let emojiString = "😊👍🏼"
print(emojiString.count)

// Output: 2
```

Merkkijonon pituuden laskeminen on myös hyödyllistä esimerkiksi silloin kun haluat tarkistaa, onko käyttäjän antama syöte sopivan pituinen tai jos haluat asettaa rajoituksia syötteelle.

## Syventyvä tarkastelu

Miksi .count metodi laskee merkkien määrän eikä vain palauta valmista arvoa? Tämä johtuu siitä, että merkkijonojen taustalla Swift käyttää Unicode-standardia, joka voi sisältää monimutkaisia merkkejä kuten esimerkiksi akkenteita, diakriittejä ja ligatuureja. Jokainen näistä merkeistä lasketaan erikseen, joten .count metodi takaa tarkan tuloksen.

## Katso myös

- [Swiftin dokumentaatio merkkijonon pituudesta](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID287)
- [Swift Playground -tutoriaali merkkijonojen käsittelystä](https://developer.apple.com/swift/blog/?id=16)