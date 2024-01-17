---
title:                "Sammanslagning av strängar"
html_title:           "Swift: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

##Vad och varför?
Strängkonkatinering är en process där man sammanfogar flera strängar till en enda sträng. Programmerare använder detta för att skapa mer komplett och läsbar kod när de behöver kombinera flera strängar tillsammans.

##Hur gör man:
Det är enkelt att konkatenera strängar i Swift. Här är ett exempel på hur man kan kombinera två strängar:
```Swift
let förnamn = "Maria"
let efternamn = "Jones"

let helaNamnet = förnamn + efternamn //helaNamnet = "Maria Jones"
```

Det är också möjligt att konkatenera en sträng med ett annat dataformat, till exempel en int:
```Swift
let ålder = 28
let meddelande = "Jag är \(ålder) år gammal." //meddelande = "Jag är 28 år gammal."
```

##Djupdykning:
Strängkonkatinering är en vanlig teknik inom programmering, och det finns flera sätt att utföra det på. I äldre versioner av Swift, användes operatorn "+" för att konkatenera strängar. Men i den senaste versionen, Swift 5, har det införts en ny operator, "&", som är mer effektiv och snabbare än den tidigare. Det finns också metoder som "append()" och "join()" som kan användas för att konkatenera strängar.

##Se även:
[Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID288)

[Swift String Concatenation Performance](https://stackoverflow.com/questions/50187161/string-concatenation-performance-in-swift-4-1)