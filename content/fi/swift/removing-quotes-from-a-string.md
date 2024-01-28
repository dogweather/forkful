---
title:                "Merkkijonosta lainausmerkkien poistaminen"
date:                  2024-01-26T03:42:08.885110-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Merkkijonosta lainausmerkkien poistaminen tarkoittaa sisältöä ympäröivien lainausmerkkien poistamista. Tämä tehdään syötteiden puhdistamiseksi, datan valmistamiseksi tallennusta varten tai tarpeettoman tekstiformaatin poistamiseksi, mikä saattaa häiritä datan käsittelyä.

## Kuinka:

Swift antaa sinun käsitellä lainausmerkkien poistotehtävän aika näppärästi. Tässä nopea esimerkki käyttäen `replacingOccurrences(of:with:)`, joka tekee juuri sitä miltä kuulostaa—vaihtaa tekstiosia joksikin muuksi, tai ei miksikään.

```swift
var quotedString = "\"Tämä on 'lainattu' merkkijono.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // Tämä on 'lainattu' merkkijono.

// Käsitteletkö yksittäisiä lainausmerkkejä? Vain vaihda hakutermiä.
quotedString = "'Tässä toinen esimerkki.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Tässä toinen esimerkki.
```

Tuloksena on lainausmerkeistä vapaita merkkijonoja, valmiina mihin tahansa, mitä sinulla on suunniteltuna seuraavaksi.

## Syväsukellus

Olemme "siivonneet" merkkijonoja näin ohjelmoinnin sarastuksesta lähtien. Varhaisina päivinä se oli enemmän muistin säästöstä ja syntaksivirheiden välttämisestä syötteitä käsiteltäessä. Tultaessa tähän päivään, kyse on hyvästä datan hygieniasta—varsinkin kun käsitellään JSONia tai valmistellaan merkkijonoja tietokantatyötä varten. Harhaanjohtava lainausmerkki voi heittää avaimen SQL-kyselyihin nopeammin kuin ehdit sanoa "syntaksivirhe".

Vaihtoehtoja? No, jos koet `replacingOccurrences(of:with:)` metodin hieman liian tavalliseksi, saatat sukeltaa säännöllisiin lausekkeisiin monimutkaisempia malleja varten tai kun haluat poistaa lainausmerkkejä vain tietyissä kohdissa. Swiftin `NSRegularExpression` luokka on ystäväsi tässä. Mutta muista, että regex voi olla kaksiteräinen miekka—voimakas, mutta joskus liioittelua.

Toteutuksen kannalta, `replacingOccurrences(of:with:)` on metodi, jonka `String` Swiftissä tarjoaa, ja se sisäisesti kutsuu monimutkaisempia merkkijonomanipulaatiofunktioita, jotka käsittelevät Unicodea ja muita nykyaikaisen tekstinkäsittelyn monimutkaisuuksia. Se on yksi niistä "yksinkertainen pinnalta, monimutkainen konepellin alla" -jutuista, jotka Swift hoitaa, jotta sinun ei tarvitse.

## Katso myös

Lisää tietoa merkkijonojen manipuloinnista Swiftissä:

- Swift-ohjelmointikieli (Merkkijonot ja merkit): [Swift.org Dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Apple Kehittäjädokumentaatio](https://developer.apple.com/documentation/foundation/nsregularexpression)

Ja jos nyt olet utelias säännöllisistä lausekkeista ja haluat testata omia mallejasi:

- Regex101: [Regex Tester ja Debugger](https://regex101.com)
