---
title:    "Swift: Sammanslagning av strängar"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför?

Att sammanfoga strängar (även kallat "concatenation") är en vanlig operation inom Swift-programmering. Det används för att kombinera flera strängar till en enda sträng, vilket kan vara användbart för att skapa dynamiska texter eller för att enklare hantera data. I detta blogginlägg ska vi titta närmare på hur man utför denna operation.

## Hur man gör

För att sammanfoga strängar i Swift kan man använda operatorn `+` eller metoden `append()`.

```Swift
let firstName = "Johan"
let lastName = "Svensson"

let fullName = firstName + " " + lastName // Output: "Johan Svensson"

var bio = "Jag heter "
bio.append(firstName)
bio.append(" och jag är en stolt programmerare.") // Output: "Jag heter Johan och jag är en stolt programmerare."
```

Som du kan se i exemplet ovan används `+` för att enkelt sammanfoga flera strängar och `append()` för att lägga till en ny sträng i slutet av en befintlig sträng. Det är också möjligt att använda `+=` för att lägga till en sträng i slutet av en annan sträng. Notera att alla dessa metoder återskapar inte den befintliga strängen, utan skapar en ny sträng som innehåller de sammanslagna strängarna.

## Djupdykning

När man sammanfogar strängar i Swift använder man sig av en process som kallas för "string interpolation". Detta innebär att man kan inkludera variabler och uttryck i en sträng genom att använda sig av `\( )` som ett slags placeholder.

```Swift
let age = 30
let sentence = "Jag är \(age) år gammal." // Output: "Jag är 30 år gammal."
```

För att undvika att behöva använda `+` flera gånger för att sammanfoga flera strängar, kan man också använda sig av `joined()` metoden.

```Swift
let fruits = ["äpple", "banan", "apelsin"]
let sentence = "Min favoritfrukt är \(fruits.joined(separator: ", "))" // Output: "Min favoritfrukt är äpple, banan, apelsin"
```

En annan användbar metod för att sammanfoga strängar är `string(from:)`, som kan användas för att omvandla andra datatyper till strängar.

```Swift
let price = 99.99
let priceString = String(format: "%.2f kr", price) // Output: "99.99 kr"
```

## Se även

- [Swift.org: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple Developer: String Cheatsheet](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html)
- [Hacking with Swift: How to concatenate strings to make one joined string](https://www.hackingwithswift.com/example-code/strings/how-to-concatenate-strings-to-make-one-joined-string)