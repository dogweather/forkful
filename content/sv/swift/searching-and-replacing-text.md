---
title:                "Swift: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

När man arbetar med programmering, är det inte ovanligt att behöva göra ändringar i sin kod. En av de vanligaste uppgifterna är att söka och ersätta text inom sitt kodprojekt. Detta kan spara tid och minimera fel när man behöver göra en stor mängd ändringar i en fil eller flera filer. 

## Så här gör man

För att söka och ersätta text i Swift kan man använda sig av funktionen `replacingOccurences(of:with:)`. Detta gör man genom att först ange den texten man vill söka efter som det första argumentet och den text man vill ersätta med som det andra argumentet. Låt oss ta ett enkelt exempel:

```Swift
let text = "Hej, jag heter *Sandra* och jag gillar *Swift*."
let newText = text.replacingOccurrences(of: "*", with: "")
```

I detta exempel söker vi efter asterisker och ersätter dem med en tom sträng. Detta skulle resultera i att `newText` blir "Hej, jag heter Sandra och jag gillar Swift." 

## Djupdykning

För att göra sökandet mer avancerat kan man använda reguljära uttryck. Detta gör det möjligt att söka efter mönster istället för en specifik textsträng. Man kan även specificera vilken del av texten som ska ersättas genom att använda ett Skriv ett reguljärt uttryck som passar dina behov. För mer information om reguljära uttryck i Swift, se Apples dokumentation [här](https://developer.apple.com/documentation/foundation/nsregularexpression).

## Se även

- [Apple Developer dokumentation för replaceOccurrences](https://developer.apple.com/documentation/foundation/nsstring/1407924-replacingoccurrences)
- [Reguljära uttryck i Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Enkel Swift tutorial om sökning och ersättning av text](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)