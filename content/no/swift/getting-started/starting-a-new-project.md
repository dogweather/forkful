---
date: 2024-01-20 18:04:49.932823-07:00
description: 'How to: (Hvordan:) Opprette et nytt prosjekt i Xcode.'
lastmod: '2024-04-05T21:53:42.102695-06:00'
model: gpt-4-1106-preview
summary: (Hvordan:) Opprette et nytt prosjekt i Xcode.
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## How to: (Hvordan:)
Opprette et nytt prosjekt i Xcode:

```Swift
// 1. Åpne Xcode.
// 2. Velg 'File' > 'New' > 'Project...'.
// 3. Velg en prosjektmal, for eksempel 'App' under 'iOS'.
// 4. Fyll inn prosjektdetaljer som produktnavn og identifikator.

// Kode i et nytt Swift-prosjekt:
import UIKit

class ViewController: UIViewController {
    override func viewDidLoad() {
        super.viewDidLoad()
        // Din kode her
        print("Hei, Norge!")
    }
}
```

Output:
```
Hei, Norge!
```

## Deep Dive (Dypdykk)
Å starte et nytt prosjekt i Swift ble enklere med Xcodes introduksjon i 2003. Alternativer til Xcode inkluderer AppCode og kodeeditorer som Visual Studio Code med Swift plugins. Detaljer viktig i oppstarten inkluderer å velge riktig målplattform (iOS, macOS, etc.), arkitektur (MVC, MVVM, etc.), og konfigurere prosjektet med nødvendige avhengigheter og moduler. Clean code-prinsipper fra start reduserer komplikasjoner senere.

## See Also (Se Også):
- [Apple's Swift Resources](https://developer.apple.com/swift/resources/)
- [Swift.org Documentation](https://swift.org/documentation/)
- [Ray Wenderlich's tutorial for starting a new project in Swift](https://www.raywenderlich.com/ios/learn)
