---
title:    "Swift: Att hitta längden på en sträng"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande färdighet i Swift-programmering. Det är användbart för att manipulera och bearbeta text, samt för att kontrollera om en sträng uppfyller vissa kriterier. Genom att lära sig hur man hittar längden på en sträng i Swift kommer du att öka dina programmeringskunskaper och kunna lösa fler problem.

## Så här gör du

För att hitta längden på en sträng, används egenskapen `count` på strängen. Detta returnerar antalet tecken i strängen. Här är ett exempel på hur man använder `count` i Swift:

```Swift
let myString = "Jag älskar Swift!"
print(myString.count)

// Output:
// 17
```

Som du kan se i exemplet ovan kommer `count` att returnera antalet tecken, inklusive mellanslag och skiljetecken.

## Djupdykning

När du arbetar med strängar i Swift är det viktigt att förstå skillnaden mellan antalet tecken och antalet bytes. Till exempel, om du har en sträng med icke-latiniska tecken, kan antalet tecken vara olika från antalet bytes. Du kan få reda på antalet bytes i en sträng genom att använda `utf8`-egenskapen:

```Swift
let myString = "🍎🍊🍇"
print(myString.count) // Output: 3
print(myString.utf8.count) // Output: 12
```

Genom att använda den här informationen kan du undvika potentiella buggar när du arbetar med internationell text.

## Se även

- Om Swift Strings i Apples dokumentation: https://developer.apple.com/documentation/swift/string
- En grundläggande överblick av Swift-programmering: https://www.apple.com/swift/
- En bok om Swift-programmering för nybörjare: https://www.amazon.com/Develop-Apps-Swift-Apple-Frameworks/dp/1491987581