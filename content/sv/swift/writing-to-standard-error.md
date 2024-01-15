---
title:                "Skriva till standardfel"
html_title:           "Swift: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att kunna skriva till standard error i din Swift-kod är en viktig färdighet för utvecklare, särskilt när du felsöker dina program. Det låter dig skriva ut specifika meddelanden eller felmeddelanden till terminalen, vilket gör det enklare att identifiera och lösa problem.

## Hur gör man

För att skriva till standard error i Swift, använder du funktionen "write" med parametern "to". Använd "stderr" för att ange att du vill skriva till standard error, och skriv sedan ut ditt meddelande inom paranteser.

```Swift 
write("Detta är ett felmeddelande!", to: &stderr)
```

Detta kommer att skriva ut "Detta är ett felmeddelande!" till standard error. Notera att "&" tecknet används för att ange en referens till standard error för funktionen.

## Djupdykning

När du skriver till standard error, skriver du egentligen till standardfelkanalen. Detta är en separat kanal där alla felmeddelanden och utdata från ditt program skrivs ut. Anledningen till att standard error används för felmeddelanden istället för standard output är för att distinkt skilja mellan normal utdata och felmeddelanden, vilket underlättar felsökningen.

## Se även

- [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Apple Developer Documentation](https://developer.apple.com/documentation/swift/2834607-write)
- [Ray Wenderlich Tutorial](https://www.raywenderlich.com/990-swift-runtime-essentials/lessons/3)