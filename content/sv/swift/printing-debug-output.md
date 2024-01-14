---
title:    "Swift: Utskrift av felutdata"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
När man arbetar med Swift-programmering är det lätt att stöta på problem och buggar. Det kan vara frustrerande att inte veta vad som orsakar problemet, särskilt om det är en komplex kod. Att använda utskrift av felsökningsinformation är ett enkelt och effektivt sätt att ta reda på vad som händer under körningen av ditt program.

## Hur man gör
För att skriva ut felsökningsinformation i Swift kan du använda funktionen `print()`. Detta kommer att skriva ut det du anger inom parenteserna i Xcode's debug area. Här är ett enkelt exempel:

```Swift
let num1 = 10
let num2 = 5
print("Summan av \(num1) och \(num2) är \(num1 + num2)")
```

Output:

```
Summan av 10 och 5 är 15
```

Du kan också skriva ut värdena av variabler och objekt genom att använda `String` initieraren. Här är ett exempel:

```Swift
let namn = "Emma"
let ålder = 25
print("Mitt namn är " + String(namn) + " och jag är " + String(age) + " år gammal.")
```

Output:

```
Mitt namn är Emma och jag är 25 år gammal.
```

## Djupdykning
En annan användbar funktion för felsökning är `dump()`. Detta kommer att skriva ut en strukturerad och detaljerad beskrivning av ett objekt, inklusive alla dess egenskaper och deras värden. Detta kan vara särskilt användbart när du arbetar med komplexa datatyper som arrayer eller dictionaries.

En annan tips för felsökning är att använda `assert()`. Denna funktion låter dig ställa in villkor och om dessa villkor inte uppfylls kommer den att skriva ut ett felmeddelande och avbryta körningen. Detta kan vara användbart för att hitta och åtgärda problem innan de orsakar en krasch.

## Se även
Här är några resurser för vidare läsning om Swift-felsökning:

- [Officiell Swift Debugging Guide](https://developer.apple.com/library/content/documentation/IDEs/Conceptual/xcode_help-command_line_developer_tools/Chapters/Xcode_Developer_Tools_Command-Line_Tools.html#//apple_ref/doc/uid/TP40002663-CH107-SW1)
- [Swift Debugging and Profiling Techniques](https://www.raywenderlich.com/26632/how-to-debug-memory-leaks-with-xcode-and-instruments-tutorial)
- [Debugging Swift: Advanced tips and techniques](https://www.hackingwithswift.com/read/7/3/debugging-swift-advanced-tips-and-techniques)