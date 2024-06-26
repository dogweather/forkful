---
date: 2024-01-20 17:37:37.325190-07:00
description: "How to: - Kuinka tehd\xE4: Date-olioista tulee stringej\xE4 melko usein,\
  \ kuten n\xE4yt\xF6ll\xE4 kertominen, tiedon tallennus ja aikaleimojen luonti lokeille.\u2026"
lastmod: '2024-04-05T22:51:11.066145-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka tehd\xE4: Date-olioista tulee stringej\xE4 melko usein, kuten n\xE4\
  yt\xF6ll\xE4 kertominen, tiedon tallennus ja aikaleimojen luonti lokeille. Historiallinen\
  \ konteksti: DateFormatter on osa Foundation frameworkia, joka tuli iOS:lle sen\
  \ ensimm\xE4isen version mukana ja on sen j\xE4lkeen palvellut Swift-kehitt\xE4\
  ji\xE4. Vaihtoehtoja: voi k\xE4ytt\xE4\xE4 my\xF6s `DateComponentsFormatter` ottaakseen\
  \ esiin vain tietyt osat p\xE4iv\xE4m\xE4\xE4r\xE4st\xE4 tai `ISO8601DateFormatter`\
  \ ISO 8601 -mukaisten merkkijonojen luomiseen. Implementation details: `DateFormatter`\
  \ k\xE4ytt\xE4\xE4 ICU-kirjastoa taustalla, mik\xE4 mahdollistaa monimutkaisten\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4formaattejen k\xE4sittelyn."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to: - Kuinka tehdä:
```Swift
import Foundation

let nyt = Date()
let muotoilija = DateFormatter()

// Suomalaiseen tapaan muotoiltu päivämäärä
muotoilija.dateFormat = "dd.MM.yyyy HH:mm"
let paivamaaraMerkkijonona = muotoilija.string(from: nyt)
print(paivamaaraMerkkijonona) // output: "29.03.2023 14:37" (esimerkki)
```

```Swift
// ISO 8601 -standardin mukainen aika ja päivämäärä
muotoilija.dateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"
let iso8601PvmMerkkijonona = muotoilija.string(from: nyt)
print(iso8601PvmMerkkijonona) // output: "2023-03-29T14:37:00+0300" (esimerkki)
```

## Deep Dive - Syväsukellus
Date-olioista tulee stringejä melko usein, kuten näytöllä kertominen, tiedon tallennus ja aikaleimojen luonti lokeille. Historiallinen konteksti: DateFormatter on osa Foundation frameworkia, joka tuli iOS:lle sen ensimmäisen version mukana ja on sen jälkeen palvellut Swift-kehittäjiä.

Vaihtoehtoja: voi käyttää myös `DateComponentsFormatter` ottaakseen esiin vain tietyt osat päivämäärästä tai `ISO8601DateFormatter` ISO 8601 -mukaisten merkkijonojen luomiseen. Implementation details: `DateFormatter` käyttää ICU-kirjastoa taustalla, mikä mahdollistaa monimutkaisten päivämääräformaattejen käsittelyn.

## See Also - Katso Myös
- Apple DateFormatter: https://developer.apple.com/documentation/foundation/dateformatter
- Swift Standard Library Date: https://developer.apple.com/documentation/swift/date
- ISO 8601 Date and Time Format: https://en.wikipedia.org/wiki/ISO_8601
