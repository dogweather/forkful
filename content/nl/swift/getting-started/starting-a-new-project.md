---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:00.132208-07:00
description: 'Hoe te: .'
lastmod: '2024-03-13T22:44:51.157561-06:00'
model: gpt-4-0125-preview
summary: .
title: Een nieuw project starten
weight: 1
---

## Hoe te:
```Swift
import SwiftUI

@main
struct NewProjectApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

struct ContentView: View {
    var body: some View {
        Text("Hallo, nieuw project!")
            .padding()
    }
}

// Voorbeelduitvoer:
// Toont een venster met de tekst "Hallo, nieuw project!".
```

## Diepgaand
Terug in de dagen vóór Swift, was Objective-C de dominante taal en het starten van een nieuw project omvatte wat meer standaardcode. Swift heeft echter het opstartproces verfijnd met nette functies zoals het `@main` attribuut, dat het startpunt van de app aanduidt. In vergelijking met hulpmiddelen zoals de sjablonen van Xcode, vereenvoudigt Swift routinetaken zodat je direct naar het leuke deel kunt springen – je idee tot leven brengen.

Wat betreft alternatieven, je zou kunnen kiezen voor een opdrachtregelhulpprogramma of een server-side framework als je geen iOS/macOS-app maakt. Qua implementatie is Swift's aanpak gericht op het minimaliseren van de initiële complexiteit. De `ContentView` vertegenwoordigt het startpunt van de UI, terwijl de `WindowGroup` het vensterbeheer afhandelt.

## Zie ook
- [Swift Documentatie](https://swift.org/documentation/)
- [Apple's SwiftUI Tutorials](https://developer.apple.com/tutorials/swiftui)
- [Begin met het Ontwikkelen van iOS Apps (Swift)](https://developer.apple.com/library/archive/referencelibrary/GettingStarted/DevelopiOSAppsSwift/)
