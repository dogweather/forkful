---
date: 2024-01-20 17:46:46.561603-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) Substringien poiminta on vanha konsepti,\
  \ mik\xE4 ly\xF6 juurensa aikojen alkuun, kun ohjelmointia alettiin kehitt\xE4m\xE4\
  \xE4n. Swiftiss\xE4\u2026"
lastmod: '2024-04-05T22:51:11.045865-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4\xE4n:) Substringien poiminta on vanha konsepti, mik\xE4\
  \ ly\xF6 juurensa aikojen alkuun, kun ohjelmointia alettiin kehitt\xE4m\xE4\xE4\
  n."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to: (Kuinka tehdään:)
```Swift
let fullString = "Hello, World!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.endIndex, offsetBy: -1)
let substring = fullString[startIndex..<endIndex] // "World"

// Tai käytä avustavia metodeja
let range = fullString.range(of: "World")!
let world = fullString[range] // "World"

print(substring) // Output: World
print(world)     // Output: World
```

## Deep Dive (Syväsukellus):
Substringien poiminta on vanha konsepti, mikä lyö juurensa aikojen alkuun, kun ohjelmointia alettiin kehittämään. Swiftissä substringit ovat kevyitä, koska ne jakavat alkuperäisen merkkijonon tallennustilan viitaten samaan muistialueeseen. Aikaisemmin käytettiin `NSString`-metodeita, mutta nyt Swift tarjoaa omia optimoituja metodeita paremman suorituskyvyn ja turvallisuuden saavuttamiseksi. Varmista käyttäessäsi, että indeksit ovat merkkijonon rajoissa, ettei tule `Index out of range` -virhettä.

## See Also (Katso myös):
- Swiftin virallinen merkkijono-opas: [Strings and Characters in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Apple Developer Documentation: [Substring](https://developer.apple.com/documentation/swift/substring)
- Tutorial "Working with Strings in Swift": [Ray Wenderlich Tutorials](https://www.raywenderlich.com/library?q=strings&sort_order=relevance)
