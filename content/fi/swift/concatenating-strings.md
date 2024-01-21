---
title:                "Merkkijonojen yhdistäminen"
date:                  2024-01-20T17:35:41.161529-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Yhdistämme merkkijonoja siksikin, että voimme luoda pitkiä tekstinpätkiä pienistä palasista. Tätä tekevät ohjelmoijat everywhere luodaakseen dynaamisia viestejä tai yhdistelläkseen käyttäjäsyötettä.

## How to: (Miten tehdään:)
```Swift
// Yksinkertainen yhdistäminen operaattorilla +
let tervehdys = "Hei, "
let nimi = "Matti!"
let yhdessa = tervehdys + nimi
print(yhdessa) // "Hei, Matti!"

// String interpolation
let ikä = 30
let esittely = "Minun nimeni on \(nimi) ja olen \(ikä) vuotta vanha."
print(esittely) // "Minun nimeni on Matti! ja olen 30 vuotta vanha."

// append-metodin käyttö
var viesti = "Kello on"
viesti.append(" 18:00.")
print(viesti) // "Kello on 18:00."
```

## Deep Dive (Syväsukellus):
Merkkijonojen yhdistely on vanha käytäntö, ja kielessä kuin kielessä löytyy siihen keinot. Ennen Swiftiä Objective-C:ssä joutui käyttämään `NSString` -luokan `stringByAppendingString`:iä, mikä ei ollut niin suoraviivaista. Swiftissä keinoja on useita, mutta kaikkein suosituinta on käyttää `+` operaattoria yksinkertaisuuden ja luettavuuden vuoksi. Interpolaatio on varteenotettava vaihtoehto, kun halutaan sisällyttää muuttujia tai laskentoja. `append`-metodi taas on hyödyllinen, kun halutaan lisätä tekstiä olemassa olevaan merkkijonoon ilman uuden luomista. Tehokkuuden kannalta merkkijonojen liittämisen voi tehdä monella tapaa, ja suorituskyvyn vaikutukset riippuvat kontekstista. 

## See Also (Katso Myös):
- [Swift Standard Library - String](https://developer.apple.com/documentation/swift/string)
- [Apple Swift Book - String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) 
- [Swift String Best Practices by Ray Wenderlich](https://www.raywenderlich.com/553-string-tutorial-for-swift-4-part-1)