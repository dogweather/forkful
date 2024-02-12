---
title:                "Merkkijonojen osien poimiminen"
aliases: - /fi/swift/extracting-substrings.md
date:                  2024-01-20T17:46:46.561603-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Substringin poimiminen on perusoperaatio, joka irrottaa osan merkkijonosta. Käytämme sitä, kun haluamme käsitellä tai näyttää vain tietyn osan merkkijonosta.

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
