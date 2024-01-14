---
title:                "Swift: Omvandling av en sträng till små bokstäver"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig uppgift som kan vara användbar för att förbättra användarens upplevelse, till exempel genom att erbjuda mer flexibilitet i sökningar eller filtreringar.

## Hur man gör det

```Swift
let originalString = "Hello, WORLD!"
let lowercasedString = originalString.lowercased()

print(lowercasedString)
```

Output: "hello, world!"

## Djupdykning

Att konvertera en sträng till gemener är en enkel uppgift i Swift tack vare `lowercased()` funktionen. Detta kan också användas tillsammans med andra strängmanipuleringsfunktioner för att åstadkomma önskad funktionalitet.

## Se även

- [Apple Developer Documentation: String Lowercasing](https://developer.apple.com/documentation/swift/string/3129814-lowercased)
- [Swift Strings Cheat Sheet](https://www.andyibanez.com/development/swift/strings-cheat-sheet/#lowercasing-strings)