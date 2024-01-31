---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
simple_title:         "Skriving av en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Skriving til tekstfil er lagring av data som tekst. Kodebruk fordi det er enkelt, menneskelesbart, og integreres pent med andre systemer.

## How to:
Oppretter en tekstfil og skriver inn data med Swift:

```Swift
import Foundation

let filnavn = "MinFil.txt"
let dokumenterUrl = try FileManager.default.url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: true)
let filUrl = dokumenterUrl.appendingPathComponent(filnavn)

let tekst = "Hei, Norge!"
do {
    try tekst.write(to: filUrl, atomically: true, encoding: .utf8)
    print("Filen ble skrevet til: \(filUrl)")
} catch {
    print("Det oppsto en feil: \(error)")
}
```

Output:

```
Filen ble skrevet til: file:///Users/dittbrukernavn/Documents/MinFil.txt
```

## Deep Dive
Tidligere skrev vi til filer med lavnivå C-funksjoner. Swift forenkler med `String` og `FileManager`. Alternativer inkluderer databaser og nøkkel-verdi lagring, men tekstfiler er fortsatt nyttige for konfigurasjon, logging, og lette dataoppgaver.

## See Also
- [FileManager Class Reference](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Standard Library on Swift.org](https://www.swift.org/documentation/#the-swift-standard-library)
