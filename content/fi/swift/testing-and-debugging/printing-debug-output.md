---
date: 2024-01-20 17:53:35.735646-07:00
description: "Debug-tuloste auttaa n\xE4kem\xE4\xE4n, mit\xE4 ohjelmaasi tapahtuu\
  \ ajon aikana. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 virheiden etsimiseen ja ohjelman\
  \ k\xE4ytt\xE4ytymisen\u2026"
lastmod: '2024-02-25T18:49:53.818012-07:00'
model: gpt-4-1106-preview
summary: "Debug-tuloste auttaa n\xE4kem\xE4\xE4n, mit\xE4 ohjelmaasi tapahtuu ajon\
  \ aikana. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 virheiden etsimiseen ja ohjelman\
  \ k\xE4ytt\xE4ytymisen\u2026"
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Debug-tuloste auttaa näkemään, mitä ohjelmaasi tapahtuu ajon aikana. Ohjelmoijat käyttävät sitä virheiden etsimiseen ja ohjelman käyttäytymisen ymmärtämiseen.

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
