---
date: 2024-01-20 18:04:39.775647-07:00
description: "How to: (Kuinka tehd\xE4:) Aloita uusi projekti Swiftiss\xE4 k\xE4ytt\xE4\
  m\xE4ll\xE4 Xcodea, Applen kehitysymp\xE4rist\xF6\xE4. T\xE4ss\xE4 on esimerkki."
lastmod: '2024-04-05T21:53:58.486739-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Aloita uusi projekti Swiftiss\xE4 k\xE4ytt\xE4m\xE4ll\xE4\
  \ Xcodea, Applen kehitysymp\xE4rist\xF6\xE4."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: (Kuinka tehdä:)
Aloita uusi projekti Swiftissä käyttämällä Xcodea, Applen kehitysympäristöä. Tässä on esimerkki.

```Swift
// 1. Avaa Xcode.
// 2. Valitse File > New > Project... (Tai paina Command-Shift-N).
// 3. Valitse sovelluspohja, esimerkiksi 'Single View App'.
// 4. Anna projektillesi nimi ja valitse tallennuspaikka.
// 5. Setup complete! Kirjoita ensimmäinen Swift-koodisi.

import SwiftUI

struct ContentView: View {
    var body: some View {
        Text("Hello, world!")
            .padding()
    }
}

@main
struct MyApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

// Käynnistä sovellus painamalla 'Run'-painiketta tai Command-R.
```

Kun olet tehnyt yllä olevat askeleet, saat näyttöön "Hello, world" -viestin.

## Deep Dive (Syväsukellus)
Swift-projektin aloittaminen juontaa juurensa ohjelmoinnin alkuaikoihin, jolloin ensimmäinen toimiva ohjelma ilmoitti "Hello, world" - maailmalle. Tämä perinne jatkuu yhä opetustarkoituksessa ja testauksessa. Vaihtoehtoina Swift projekteille voisi olla muut kielet ja kehitysympäristöt, kuten Python PyCharmilla tai JavaScript Visual Studio Codella. Swiftiä ja sen työympäristöä suositaan erityisesti iOS-, macOS-, watchOS- ja tvOS-sovellusten kehityksessä sen tehokkuuden, turvallisuuden ja Apple-alustan integraation ansiosta.

## See Also (Katso myös)
- Apple's Swift Resources: [https://developer.apple.com/swift/resources/](https://developer.apple.com/swift/resources/)
- Swift Documentation by Apple: [https://docs.swift.org/swift-book/](https://docs.swift.org/swift-book/)
- Swift Playgrounds for iPad: [https://www.apple.com/swift/playgrounds/](https://www.apple.com/swift/playgrounds/)
