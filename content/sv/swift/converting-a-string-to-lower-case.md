---
title:    "Swift: Konvertera en sträng till små bokstäver"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför
Stringar är en vanlig datatyp som används i Swift, och de kan innehålla en rad olika tecken och symboler. Ibland kan det vara användbart att konvertera en sträng till små bokstäver för att underlätta jämförelser och sökningar.

## Så här gör du
För att konvertera en sträng till små bokstäver, kan du använda metoden `lowercased()`. Med denna metod kommer alla bokstäver i strängen att omvandlas till små bokstäver.

```Swift
let string1 = "SWIFT PROGRAMMERING"
let lowercaseString = string1.lowercased()
print(lowercaseString) // swift programmering
```

Du kan även använda metoden `capitalized`, som omvandlar den första bokstaven i varje ord till stor bokstav och resten av bokstäverna till små.

```Swift
let string2 = "välkommen till swift"
let capitalizedString = string2.capitalized
print(capitalizedString) // Välkommen Till Swift
```

Om du endast vill omvandla den första bokstaven i strängen till stor bokstav, kan du använda metoden `prefix(1)` tillsammans med `capitalized`. 

```Swift
let string3 = "låt oss lära oss swift"
let firstLetter = string3.prefix(1).capitalized
print(firstLetter) // L
```

## Deep Dive
När du använder metoden `lowercased()` så omvandlas även alla specialtecken och unicode-tecken i strängen till små bokstäver. Detta kan vara användbart vid jämförelser där specialtecken inte spelar någon roll.

En annan viktig sak att tänka på är att metoden `lowercased()` inte bara tar hänsyn till de engelska alfabetet, utan den tar också hänsyn till andra språk där bokstäver kan skrivas på olika sätt. Till exempel, i tyska språket så åker diakritiska tecken såsom "ä", "ö" och "ü" bort vid konvertering till små bokstäver.

## Se även
- [Apple's dokumentation om `lowercased()`](https://developer.apple.com/documentation/swift/string/3126799-lowercased)
- [En artikel om strängmanipulering i Swift](https://www.avanderlee.com/swift/string-manipulation-swift/)
- [En tutorial om grundläggande Swift-programmering](https://blog.sundell.com/2020/01/22/basic-swift-programming-tutorials/)