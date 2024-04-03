---
date: 2024-01-20 17:53:35.735646-07:00
description: "How to: (Miten:) Swiftiss\xE4 tulostaminen konsoliin k\xE4y `print`-funktiolla.\
  \ Katsotaan pari esimerkki\xE4."
lastmod: '2024-03-13T22:44:56.909493-06:00'
model: gpt-4-1106-preview
summary: "Swiftiss\xE4 tulostaminen konsoliin k\xE4y `print`-funktiolla."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to: (Miten:)
Swiftissä tulostaminen konsoliin käy `print`-funktiolla. Katsotaan pari esimerkkiä.

```Swift
// Yksinkertainen tulostus
print("Hei maailma!")

// Muuttujien yhdistäminen merkkijonoon
let hedelma = "omena"
print("Minulla on \(hedelma).")
```

Esimerkkien tulostus konsolissa näyttää tältä:

```
Hei maailma!
Minulla on omena.
```

## Deep Dive (Syväsukellus)
Historiallisesti tulostaminen on ollut perusväline debuggauksessa. Swiftissä `print` on suora tapa päästä käsiksi, mutta on olemassa myös muita keinoja kuten `debugPrint` tai Unified Logging System. `debugPrint` antaa tarkemmat tiedot, ja Unified Logging System sallii eri tasoisten logien merkitsemisen. 

Kun tulostat suuria tietorakenteita, `dump`-funktio voi olla avuksi, koska se näyttää enemmän sisäistä rakennetta. 

```Swift
struct Henkilo {
    let nimi: String
    let ika: Int
}

let henkilo = Henkilo(nimi: "Jenna", ika: 28)
dump(henkilo)
```

Tulostus antaa sinulle tietorakenteen hierarkisen näkymän:

```
▿ Henkilo
  - nimi: "Jenna"
  - ika: 28
```

## See Also (Katso Myös)
- Apple Developer Documentation for `print`: <https://developer.apple.com/documentation/swift/1541053-print>
- Apple's Unified Logging Documentation: <https://developer.apple.com/documentation/os/logging>
- WWDC video about logging: "Unified Logging and Activity Tracing" <https://developer.apple.com/videos/play/wwdc2016/721/>
