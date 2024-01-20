---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & varför?

Att ta bort tecken som matchar ett mönster är ett programmeringsteknik som hittar en viss sekvens av tecken och eliminerar dem från en sträng. Programmerare gör detta för att rensa eller formatera data mer effektivt.

## Så här gör du:

Följande exempel visar hur du tar bort alla siffror från en sträng i Swift:

```Swift 
let str = "abc123def456"
let trimmed = str.filter { !"0123456789".contains($0) }
print(trimmed)
```

När du kör den här koden blir utmatningen: `abcdef`

## Fördjupning

Håll i dig nu, för vi ska dyka djupare! Att ta bort tecken som matchar ett mönster har historiska rötter tillbaka till tidigare programmeringsspråk som Perl och JavaScript, där det ofta användes för strängmanipulation och datarensning.

Swift tillhandahåller flera alternativ för att utföra samma operation. Till exempel kan du använda `ReplacingOccurrences` funktionen:

```Swift
let replaced = str.replacingOccurrences(of: "\\d", with: "", options: .regularExpression)
print(replaced)
```

Medan `filter`-metoden går igenom varje tecken i strängen och kontrollerar om det finns i matchsträngen, använder `replacingOccurrences` regular expressions för att hitta och ersätta matchande sekvenser.

## Se även

För att fördjupa dig mer i Swifts stränghantering, ta en titt på dessa länkar:

1. Apples dokumentation om strängar och karaktärer: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
3. Ray Wenderlich's artikel om strängmanipulation i Swift: [https://www.raywenderlich.com/449-swift-string-cheat-sheet-and-quick-reference](https://www.raywenderlich.com/449-swift-string-cheat-sheet-and-quick-reference)