---
title:                "Jonojen yhdistäminen"
html_title:           "Swift: Jonojen yhdistäminen"
simple_title:         "Jonojen yhdistäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi yhdistää tekstejä Swift-ohjelmointikielessä? Yhdistäminen on hyödyllinen työkalu kun halutaan luoda dynaamisia ja muokattavia tekstejä, kuten käyttäjän syötteen lisääminen valmiiseen lauseeseen.

## Kuinka

```Swift
let firstName = "Jenna"
let lastName = "Smith"
let greeting = "Hei " + firstName + " " + lastName + ". Tervetuloa!"
print(greeting)
```
Tuloste:
```
Hei Jenna Smith. Tervetuloa!
```

Yhdistääksemme tekstejä Swiftissä, käytämme + operaattoria. Voimme yhdistää useita tekstejä yhteen, joko puhdistamalla ne erillisillä + -operaattoreilla tai käyttämällä välilyöntiä helpottamaan lukemista. Voimme myös yhdistää tekstin muuttujien kanssa, mikä tekee tekstin muokkaamisesta ja personointimahdollisuuksista helpompaa.

```Swift
let number = 42
let message = "Vastasit oikein " + String(number) + " kysymykseen."
print(message)
```
Tuloste:
```
Vastasit oikein 42 kysymykseen.
```

## Deep Dive

Swiftissä tekstin yhdistäminen käyttäen + operaattoria hyödyntää taustalla `String`-luokkaa ja sen `init`-metodia. Tämä metodi luo uuden merkkijonon kahden olemassaolevan merkkijonon avulla ja yhdistää ne toisiinsa.

Vaikka + operaattori on kätevä tapa yhdistää tekstejä, se ei ole tehokkain vaihtoehto suurissa tekstikokoelmissa. Tässä tapauksessa on suositeltavaa käyttää `String`-luokan `append`-metodia tai `+` operaattorin sijaan `+=` -operaattoria, joka muokkaa alkuperäistä merkkijonoa sen sijaan, että loisi uuden.

## Katso myös

- [Stringin dokumentaatio](https://developer.apple.com/documentation/swift/string)
- [Swiftin keskustelu merkkijonojen yhdistämisestä](https://forums.swift.org/t/what-is-the-performance-impact-of-using-operator-vs-append-operator-to-concatenate-strings/842)
- [Lyhyt opas Markdown-syntaksiin](https://www.markdownguide.org/basic-syntax/)