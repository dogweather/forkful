---
title:                "Swift: Sammanslagning av strängar"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar, eller concatenating strings, är en viktig färdighet inom Swift-programmering. Det låter dig kombinera flera separata strängar till en enda sträng. Detta är särskilt användbart när du vill skapa dynamiskt genererade textsträngar, till exempel när du hämtar data från en databas eller användarens input.

## Så här gör du

För att sammanslå två strängar i Swift använder du operatorn +. Låt oss säga att du har två strängar, "Hej" och "världen". Om du vill kombinera dem till en enda sträng använder du följande syntax:

```Swift
let str1 = "Hej"
let str2 = "världen"
let combinedStr = str1 + " " + str2
print(combinedStr)
```

Output: Hej världen

Som du kan se i exemplet ovan använde vi + operatorn för att lägga till ett mellanslag mellan de två strängarna. Detta gör att de två strängarna fogas samman till en enda sträng, "Hej världen". Du kan också använda += operatorn för att lägga till en sträng till en befintlig variabel.

```Swift
var greeting = "Hej"
greeting += " världen"
print(greeting)
```

Output: Hej världen

## Djupdykning

När du sammanslår strängar i Swift är det viktigt att vara medveten om data-typerna. Till exempel, om du försöker kombinera en sträng med en bool-värde, kommer du att få ett felmeddelande eftersom Swift inte kan avgöra hur man kombinerar dessa olika datatyper. I sådana fall kan du använda funktionen String() för att konvertera datatyper till strängar innan du sammanslår dem.

En annan viktig aspekt att tänka på är hur sammanslagna strängar påverkar prestandan. Om du kombinerar flera strängar i en loop kan det leda till en långsam körningstid. I sådana fall kan det vara bättre att använda en strängbuffert eller String interpolation för att förbättra prestandan.

## Se även

- [Swift Documentation: String and Character](https://developer.apple.com/documentation/swift/string)
- [Hacking with Swift: Concatenating strings with appendString()](https://www.hackingwithswift.com/example-code/strings/how-to-concatenate-strings-to-make-one-joined-string)