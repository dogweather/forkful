---
title:                "Swift: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort eller ersätta tecken som matchar ett visst mönster är ofta en nödvändig del av Swift-programmering. Oavsett om det handlar om att rensa formatering från en textsträng eller filtrera ut ogiltiga tecken, finns det många scenarier där denna typ av manipulation är användbar.

## Så här gör du

För att ta bort karaktärer som matchar ett visst mönster i Swift finns det flera metoder och funktioner tillgängliga. Nedan är några exempel och sample output kodblock som visar hur man kan använda dem:

```Swift
let sentence = "Hej världen!"

// Ta bort sista karakteren från stringen
let updatedSentence = sentence.dropLast()
// output: "Hej världen"

// Ta bort alla vokaler från stringen
let updatedSentence = sentence.filter({ !["a", "e", "i", "o", "u", "y"].contains($0) })
// output: "Hj vrldn!"

// Ersätta alla siffror med "*"
let updatedSentence = sentence.replacingOccurrences(of: "[0-9]", with: "*", options: .regularExpression)
// output: "Hej världen!"
```

Som du kan se är det möjligt att använda olika metoder beroende på vilken typ av manipulation som behövs. Genom att använda en kombination av "dropLast()", "filter()" och "replacingOccurrences" kommer du att kunna ta bort eller ersätta önskade karaktärer.

## Djupdykning

En viktig del av att ta bort karaktärer som matchar ett visst mönster är att förstå hur reguljära uttryck fungerar. Reguljära uttryck (regular expressions) är en serie av symboler som används för att matcha karaktärsmönster i en sträng. De utgör en viktig del av Swifts "replacingOccurrences" metod som använder reguljära uttryck för att ersätta karaktärer i en sträng.

En annan viktig funktion för att ta bort karaktärer i Swift är "trimmingCharacters(in:)". Denna funktion används för att ta bort specifika karaktärer från början och slutet av en sträng. Till exempel kan du använda denna funktion för att ta bort alla mellanslag i början och slutet av en sträng, vilket kan vara användbart när man hanterar användarinput.

## Se även

För mer information om reguljära uttryck och hur man använder dem i Swift, kolla in dessa resurser:

- Apple's officiella "Using Regular Expressions in Swift" guide: <https://developer.apple.com/documentation/foundation/nsregularexpression>
- En tutorial på howtobuildsoftware.com om hur man använder reguljära uttryck i Swift: <https://www.howtobuildsoftware.com/index.php/how-do/b46F/swift-regex-swift-working-with-regular-expressions-in-swift>

Lycka till med att ta bort karaktärer som matchar ett visst mönster i dina Swift-projekt!