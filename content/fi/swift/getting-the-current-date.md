---
title:    "Swift: Hanki nykyinen päivämäärä"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi
Miksi kukaan haluaisi hakea nykyisen päivämäärän käyttämällä Swift-ohjelmointia? Päivämäärän hakeminen on tärkeä osa monia erilaisia ohjelmointitehtäviä, kuten aikaleimojen lisäämistä tietokantaan tai tapahtumien järjestämistä.

## Miten
Tässä esimerkissä näytämme kuinka voit hakea nykyisen päivämäärän Swiftillä ja tulostaa sen konsoliin.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let today = Date()
let dateString = dateFormatter.string(from: today)
print(dateString)
```

Tämä koodi luo ensin DateFormatter-olion, joka mahdollistaa päivämäärän muotoilun halutulla tavalla. Sitten se määrittää halutun muotoilun ja hakee nykyisen päivämäärän Date-muodossa. Lopuksi se muuntaa tämän päivämäärän halutunlaiseen merkkijonoon ja tulostaa sen konsoliin.

Tulostus: 27.04.2021

## Syväsukellus
Päivämäärän hakeminen Swiftillä onnistuu myös eri aikavyöhykkeiltä ja lokalisoinnista riippumatta. Voit esimerkiksi käyttää eri formaatteja, kuten "yyyy" vuodessa tai "MM" kuukaudessa.

Voit myös säätää DateFormattera-olion avulla asetuksia, kuten kellonajan näyttämistä ja kalenterityyppiä. Tämä mahdollistaa joustavan päivämäärän hakemisen erilaisissa käyttötapauksissa.

## Katso myös
- [Swiftin virallinen dokumentaatio Date-muodosta](https://developer.apple.com/documentation/foundation/date)
- [Opi lisää DateFormatterin käytöstä](https://www.codementor.io/@bretthicks/nsdateformatter-tutorial-how-to-convert-date-mcadfcep)