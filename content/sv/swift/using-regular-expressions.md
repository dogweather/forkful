---
title:    "Swift: Att använda reguljära uttryck"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att diskutera hur vi kan använda reguljära uttryck i Swift-programmering. Reguljära uttryck är ett kraftfullt verktyg för att hantera strängar och mönstermatchning. Genom att lära sig använda dem kan du effektivisera ditt kodande och göra dina program mer flexibla.

## Hur man gör

För att använda reguljära uttryck i Swift, behöver vi först importera RegularExpressions-biblioteket. Sedan kan vi skapa ett reguljärt uttryck genom att använda den inbyggda `NSRegularExpression`-klassen:

```Swift
import RegularExpressions

let pattern = "^[0-9]+\\s[a-z]{2}$"
let regex = try! NSRegularExpression(pattern: pattern) 
```

I detta exempel skapar vi ett reguljärt uttryck som matchar en sträng som börjar med en eller flera siffror, följt av ett mellanslag och två små bokstäver.

För att matcha en sträng mot vårt reguljära uttryck, använder vi `matches(_:_:)`-funktionen och skickar in både det reguljära uttrycket och strängen vi vill matcha:

```Swift
let string = "123 ab"
let matches = regex.matches(in: string, options: [], range: NSRange(location: 0, length: string.utf16.count))

// matches borde nu innehålla en `NSTextCheckingResult` som motsvarar vår matchning.
```

## Djupdykning

Reguljära uttryck kan användas för mycket mer än bara mönstermatchning. Du kan också använda dem för att göra sök- och ersättningsoperationer i strängar, validera användarinput och mer.

En användbar funktion för sök och ersättning är `stringByReplacingMatches(in:options:range:withTemplate:_:)` som låter dig ersätta en del av en sträng som matchar ett reguljärt uttryck med en given mallsträng. Detta kan vara användbart när du till exempel vill formatera indata som en användare skriver in.

```Swift
let input = "ABC123"
let pattern = "[A-Z]+([0-9]+)"
let regex = try! NSRegularExpression(pattern: pattern)

// Ersätt alla förekomster av ett ord följt av en sifferdel med motsvarande sifferdel följt av en punkt.
let result = regex.stringByReplacingMatches(in: input, options: [], range: NSRange(location: 0, length: input.utf16.count), withTemplate: "$1.")

// result borde nu vara "123."
```

## Se även

- [Apple's dokumentation om reguljära uttryck i Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex tutorial på tutorialspoint.com](https://www.tutorialspoint.com/swift/swift_regular_expressions.htm)
- [Swift by Sundell: Effektiv strängmanipulering med Regular Expressions](https://www.swiftbysundell.com/articles/regular-expressions-in-swift/)