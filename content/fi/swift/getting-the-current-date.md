---
title:                "Swift: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia käytännön syitä, miksi tarvitsisimme nykyisen päivämäärän ohjelmoinnissa. Esimerkiksi lomakkeiden täyttämisessä tai kalenteri- ja aikaa herkällä sovelluksissa, kuten varaussovelluksessa, on tärkeää saada luotettava ja ajantasainen päivämäärätieto. Onneksi Swift-ohjelmointikielessä on helppo hakea nykyinen päivämäärä.

## Miten

Swift tarjoaa Date-luokan, joka sisältää kaikki päivämäärätietoihin liittyvät toiminnot. Aluksi meidän täytyy importata Foundation-kirjasto, joka sisältää Date-luokan.

```Swift
import Foundation
```

Sitten voimme käyttää Date()-funktiota luodaksemme uuden päivämääräolion. Tämä päivämääräolion sisältämät tiedot ovat nykyinen aika ja päiväys.

```Swift
let currentDate = Date()
```

Voimme myös muuttaa päivämäärän muotoa haluamallamme tavalla käyttämällä DateFormatter-luokkaa. Se tarjoaa mahdollisuuden asettaa halutun muodon ja kielen päivämäärään. Esimerkiksi jos haluamme näyttää päivämäärän englanniksi ja muodossa "dd/MM/yyyy", voimme tehdä sen seuraavalla tavalla.

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
formatter.locale = Locale(identifier: "en")
let formattedDate = formatter.string(from: currentDate)
print(formattedDate) // tulostaa "04/03/2021"
```

## Syvällinen sukellus

Date-luokka perustuu Gregorian-kalenteriin, joka on yleisimmin käytetty kalenterijärjestelmä. Se tallentaa päivämäärän sisäisesti sekunteina alkaen 1. tammikuuta 2001. Tämä tarkoittaa, että se ei ota huomioon aikavyöhyke- tai päiväntasaajan muutoksia, joten päivämäärän tarkkuus voi joskus olla kyseenalainen. 

Voimme myös käyttää DateComponents-luokkaa saadaksemme tarkempaa tietoa päivämäärästä. Se antaa meille pääsyn päivän, kuukauden, vuoden ja viikonpäivän tietoihin erikseen.

```Swift
let calendar = Calendar.current
let components = calendar.dateComponents([.day, .month, .year, .weekday], from: currentDate)
print(components.day) // tulostaa nykyisen päivän numeron
print(components.month) // tulostaa nykyisen kuukauden numeron
print(components.year) // tulostaa nykyisen vuoden numeron
print(components.weekday) // tulostaa nykyisen viikonpäivän numeron
```

## Katso myös

- [Official Swift Date Documentation](https://developer.apple.com/documentation/foundation/date)
- [DateFormatter-luokka](https://developer.apple.com/documentation/foundation/dateformatter)
- [DateComponents-luokka](https://developer.apple.com/documentation/foundation/datecomponents)