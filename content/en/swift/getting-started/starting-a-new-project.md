---
date: 2024-01-20 18:04:29.139305-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:45:00.397069-06:00'
model: gpt-4-1106-preview
summary: .
title: Starting a new project
weight: 1
---

## How to:
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
        Text("Hello, new project!")
            .padding()
    }
}

// Sample output:
// Displays a window with "Hello, new project!" text.
```

## Deep Dive
Back in the pre-Swift days, Objective-C ruled the roost and starting a new project involved a bit more boilerplate. Swift, however, refined the start-up process with neat features like the `@main` attribute, which identifies the app's entry point. Compared with tools like Xcode's templates, Swift simplifies mundane tasks so you can jump straight to the fun part – bringing your idea to life.

As for alternatives, you might go for a command-line tool or a server-side framework if you're not making an iOS/macOS app. Implementation-wise, Swift's approach is about minimizing initial complexity. The `ContentView` represents the UI's starting point, while the `WindowGroup` handles the window management.

## See Also
- [Swift Documentation](https://swift.org/documentation/)
- [Apple's SwiftUI Tutorials](https://developer.apple.com/tutorials/swiftui)
- [Start Developing iOS Apps (Swift)](https://developer.apple.com/library/archive/referencelibrary/GettingStarted/DevelopiOSAppsSwift/)
