---
title:    "Swift: Sökning och ersättning av text"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Text sök och ersättning är en grundläggande funktion inom programmering som gör det möjligt att snabbt och enkelt ändra text i dina program. Genom att behärska denna teknik kan du spara tid och göra dina koder mer effektiva.

## Så här gör du

För att söka och ersätta text i Swift kan du använda dig av funktionen `.replacingOccurrences(of:with:)`. Detta tillåter dig att ange vilken text du vill söka efter och vad du vill ersätta den med. Koden nedan visar ett enkelt exempel på hur du kan använda denna funktion:

```Swift
var text = "Hej världen!"
let nyText = text.replacingOccurrences(of: "världen", with: "alla")
print(nyText)
```

Output: Hej alla!

## Djupdykning

För att förstå mer om sökning och ersättning inom Swift, är det viktigt att känna till att denna funktion är skiftlägeskänslig. Det betyder att om du söker efter "Hej" kommer det endast att ersätta det ordet och inte "hej" eller "HEJ". Du kan också använda dig av regex (regular expressions) för mer avancerad sökning och ersättning. Genom att lära dig mer om reguljära uttryck i Swift, kan du göra mer avancerade textmanipulationer inom dina program.

## Se även

1. [Swift Language Guide](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
2. [Swift Regular Expressions](https://pspdfkit.com/blog/2018/swift-regular-expressions/)
3. [Swift String Manipulation](https://learnappmaking.com/swift-string-how-to/)