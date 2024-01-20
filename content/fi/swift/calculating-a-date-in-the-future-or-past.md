---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Swift: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Swiftin Ajanlasku: Miten Ligsiät tai Menneet Päivät Määritellään? 

## Mitä & Miksi?

Ajanlaskulla viitataan tulevaisuuden tai menneisyyden päivän määrittämiseen. Ohjelmoijat tekevät tämän esim. lomakkeiden voimassaolon, aikajänneestimaattien tai aikataulujen luomiseksi.

## Miten se tehdään:

Alla Swift-koodi joka näyttää, miten määritellään 5 päivää tulevaisuudessa:
```Swift
let now = Date()
let fiveDaysLater = Calendar.current.date(byAdding: .day, value: 5, to: now)
print(fiveDaysLater)
```
Ja vastaavasti 5 päivää menneisyydessä:
```Swift
let fiveDaysBefore = Calendar.current.date(byAdding: .day, value: -5, to: now)
print(fiveDaysBefore)
```
Kummassakin esimerkissä päivämäärä luodaan nykyhetkestä (`now`), ja siihen lisätään tai siitä vähennetään päiviä käyttäen `Calendar.current.date(byAdding:value:to:)` -funktiota.

## Syventyminen

1. Historiallinen Konteksti: Ennen tietokoneita ajanlasku tehtiin käsin, kalenterin avulla, joka oli aikaa vievää ja virhealtista. Ohjelmoinnissa kuitenkin, voidaan laskea päiviä, kuukausia tai vuosia tarkasti ja vaivattomasti.

2. Vaihtoehdot: Swiftin lisäksi voit tehdä ajanlaskun missä tahansa ohjelmointikielessä, kuten JavaScript tai Python. Jokaisella kielellä on oma tapansa ja syntaksinsa.

3. Toteutuksen Yksityiskohdat: Huomaa, että koska Swift käyttää Gregoriaanista kalenteria, se ottaa huomioon karkauspäivät automaattisesti. Se myös käsittelee aikavyöhykkeet ja kesäaika asetukset.

## Katso Myös

- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation: Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift.org - Swiftin virallinen dokumentaatio](https://swift.org/documentation/)

Muistathan, että ohjelmoinnin oppiminen on jatkuvaa prosessia. Aina on jotain uutta ja jännittävää opittavaa. Onnea matkaan Swiftin maailmaan!