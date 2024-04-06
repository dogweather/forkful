---
date: 2024-01-20 17:43:04.326707-07:00
description: "Hur man g\xF6r: I exemplet skapar vi ett `NSRegularExpression` objekt\
  \ f\xF6r att matcha siffror och anv\xE4nda `stringByReplacingMatches` f\xF6r att\
  \ ta bort dem fr\xE5n\u2026"
lastmod: '2024-04-05T21:53:39.573141-06:00'
model: gpt-4-1106-preview
summary: "I exemplet skapar vi ett `NSRegularExpression` objekt f\xF6r att matcha\
  \ siffror och anv\xE4nda `stringByReplacingMatches` f\xF6r att ta bort dem fr\xE5\
  n str\xE4ngen."
title: "Ta bort tecken som matchar ett m\xF6nster"
weight: 5
---

## Hur man gör:
```Swift
let originalString = "Det var en solig dag i maj17, 2023!"
let pattern = "[0-9]"
let regex = try! NSRegularExpression(pattern: pattern, options: [])

let range = NSRange(location: 0, length: originalString.utf16.count)
let modifiedString = regex.stringByReplacingMatches(in: originalString, options: [], range: range, withTemplate: "")

print(modifiedString)  // "Det var en solig dag i maj, !"
```

I exemplet skapar vi ett `NSRegularExpression` objekt för att matcha siffror och använda `stringByReplacingMatches` för att ta bort dem från strängen. Resultatet är en renad sträng.

## Djupdykning
Historiskt sett har reguljära uttryck varit verktyget för att manipulera strängar i många programmeringsspråk. Swift erbjuder ett kraftfullt `NSRegularExpression` klass som är arvet från Objective-C. Det finns alternativ som att använda `String` metoderna `replacingOccurrences` om mönstret är enkelt. Dock, när det kommer till komplexa mönster, är `NSRegularExpression` ovärderlig trots sin högre komplexitet. Vid implementering av mönsterborttagning, bör man vara medveten om prestanda, särskilt vid stora textmängder.

## Se även
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
