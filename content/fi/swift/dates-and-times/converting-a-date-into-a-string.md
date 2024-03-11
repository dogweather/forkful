---
date: 2024-01-20 17:37:37.325190-07:00
description: "Muunnetaan p\xE4iv\xE4m\xE4\xE4r\xE4 merkkijonoksi jotta se olisi helpompi\
  \ n\xE4ytt\xE4\xE4 k\xE4ytt\xE4j\xE4lle. T\xE4m\xE4 tehd\xE4\xE4n k\xE4ytt\xE4j\xE4\
  yst\xE4v\xE4llisten muotojen ja paikallisten\u2026"
lastmod: '2024-03-11T00:14:30.959469-06:00'
model: gpt-4-1106-preview
summary: "Muunnetaan p\xE4iv\xE4m\xE4\xE4r\xE4 merkkijonoksi jotta se olisi helpompi\
  \ n\xE4ytt\xE4\xE4 k\xE4ytt\xE4j\xE4lle. T\xE4m\xE4 tehd\xE4\xE4n k\xE4ytt\xE4j\xE4\
  yst\xE4v\xE4llisten muotojen ja paikallisten\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Muunnetaan päivämäärä merkkijonoksi jotta se olisi helpompi näyttää käyttäjälle. Tämä tehdään käyttäjäystävällisten muotojen ja paikallisten aikavyöhykkeiden näyttämiseksi.

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
