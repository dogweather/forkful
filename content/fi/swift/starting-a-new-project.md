---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Swift-pohjainen uuden projektin aloittaminen

## Mikä & Miksi?

Uuden projektin aloitus on sovelluksen, verkkosivun tai minkä tahansa ohjelmiston luomisen aloitusprosessi. Ohjelmoijat tekevät tämän tuottaakseen uusia digitaalisia tuotteita tai parantaakseen jo olemassa olevia.

## Näin se tehdään:

Aloitetaan uuden Swift-projektin luomisprosessi Xcodella.

```Swift
// Avaa Xcode ja valitse "Uusi projekti"
// Valitse "App" mallipohjakokoelmasta ja napsauta "Next"
// Anna projektisi nimi kohteeseen "Product Name", esimerkiksi "HelloWorld"
// Valitse "StoryBoard" Interface-kohdassa ja "UIKit App Delegate" Life Cycle kohdassa
// Valitse sijainti projektillesi ja napsauta "Luo"
```

```Swift
import UIKit

// Tämä on "AppDelegate.swift" tiedoston koodi
class AppDelegate: UIResponder, UIApplicationDelegate {
    var window: UIWindow?

    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        window = UIWindow(frame: UIScreen.main.bounds)
        window?.rootViewController = ViewController()
        window?.makeKeyAndVisible()
        
        return true
    }
}
```

```Swift
import UIKit

// Tämä on "ViewController.swift" tiedoston koodi
class ViewController: UIViewController {
    override func viewDidLoad() {
        super.viewDidLoad()
        
        view.backgroundColor = .blue
    }
}
```

Kun ajat tämän koodin Xcode-simulaattorilla, sinun pitäisi nähdä sininen näyttö. Se osoittaa, että olet onnistuneesti luonut uuden Swift-projektin.

## Syvempi sukellus

Vaikka Swift on melko uusi kieli (julkaistu vuonna 2014), se on jo saavuttanut laajan suosion ohjelmoijien keskuudessa sen tehokkuuden, turvallisuuden ja helppouden ansiosta. Swiftin aloittaminen uutena projektina on suoraviivainen prosessi Xcode-kehitysympäristössä.

Vaihtoehtoja Swift-projektin aloittamiseen ovat esimerkiksi Objective-C tai jopa C++, mutta Swift on moderni ja intuitiivinen. Se noudattaa LLVM-kompilaattoria ja sen ARC:n muistinhallintaa, Swift on suorituskyvyltään ja nopeudeltaan vaikuttava.

## Katso myös

1. [Virallinen Swift-kielen opas](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
2. [Xcode-ohjelmiston perusteet](https://developer.apple.com/library/archive/referencelibrary/GettingStarted/DevelopiOSAppsSwift/)
3. [Apple Developer - Swift](https://developer.apple.com/swift/)

---