---
date: 2024-01-20 17:58:51.489223-07:00
description: "S\xF6k och ers\xE4tt av text \xE4r en grundl\xE4ggande process d\xE4\
  r vi hittar specifika teckenkombinationer och byter ut dem mot nya. Programmerare\
  \ anv\xE4nder det f\xF6r\u2026"
lastmod: '2024-03-13T22:44:38.236747-06:00'
model: gpt-4-1106-preview
summary: "S\xF6k och ers\xE4tt av text \xE4r en grundl\xE4ggande process d\xE4r vi\
  \ hittar specifika teckenkombinationer och byter ut dem mot nya."
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## Så här gör du:
Här är ett snabbt exempel i Swift:

```Swift
var text = "Hej, jag heter Ada!"
let searchText = "Ada"
let replacementText = "Lee"

if let range = text.range(of: searchText) {
    text.replaceSubrange(range, with: replacementText)
}

print(text)  // "Hej, jag heter Lee!"
```

Det ska vara enkelt. Koden hittar `searchText` i `text` och byter ut det mot `replacementText`.

## Djupdykning
Vid textbearbetning är sök och ersätt lika gammal som ordbehandlare. I programmering, har vi gått från enkla kommandon till komplexa bibliotek som hanterar allt från enkel text till reguljära uttryck.

I Swift är `String` -typen rik på metoder för textmanipulering. För enklare fall som exemplet ovan använder vi `range(of:)` och `replaceSubrange(_:with:)`. För mer invecklade mönster använder vi reguljära uttryck via `NSRegularExpression`.

Det finns alternativ för att arbeta med strängar i Swift. Du kan använda:

- `replacingOccurrences(of:with:)` för enklare ersättningar.
- `NSMutableString` som ger muterbara strängar om du hanterar stora datamängder.

Varje metod har sina användningsområden, och det är nyckeln till att välja rätt verktyg för jobbet.

## Se även:
För vidare läsning och mer detaljerad information:

- Swift documentation on Strings: [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- Working with Regular Expressions in Swift: [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
