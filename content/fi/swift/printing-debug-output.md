---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:53:35.735646-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/printing-debug-output.md"
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
