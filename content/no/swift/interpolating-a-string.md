---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:51:35.531592-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Stringinterpolasjon lar deg sette variabler rett inn i strenger. Det forenkler kombinasjonen av statisk tekst og variabler, noe som gjør koden mer leselig og vedlikeholdbar.

## How to:
```Swift
let name = "Ola"
let welcomeMessage = "Hei, \(name)!"
print(welcomeMessage)
```
Output: `Hei, Ola!`

```Swift
let apples = 3
let oranges = 5
let fruitSummary = "Jeg har \(apples + oranges) frukter totalt."
print(fruitSummary)
```
Output: `Jeg har 8 frukter totalt.`

## Deep Dive
Stringinterpolasjon har vært en del av Swift siden begynnelsen i 2014. Alternativer inkluderer sammenslåing av strenger med `+` og formatfunksjoner som `String(format:)`, men disse kan være klønete. Interpolasjon er implisitt, så du trenger ikke en ekstra funksjon eller operatør. Teknisk sett bruker Swift `CustomStringConvertible` protokollen, som lar typer definere hvordan de skal omdannes til strenger. Dette gjør interpolasjon svært kraftig, siden det kan håndtere nesten hvilken som helst datatype ved å automatisere konverteringsprosessen basert på typens implementasjon av `description` egenskapen.

## See Also
- The Swift Programming Language (String Interpolation): [Swift Docs](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- Customizing String Interpolation in Swift 5: [Hacking with Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5)
