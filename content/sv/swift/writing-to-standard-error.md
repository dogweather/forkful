---
title:                "Swift: Skriva till standardfel"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error i Swift kan vara en användbar teknik för att felmeddelanden och varningar ska synas på ett tydligt sätt. Detta gör det möjligt för utvecklare att snabbt felsöka och förbättra sin kod.

## Hur man gör det

Det finns flera sätt att skriva till standard error i Swift, men ett enkelt sätt är att använda funktionen `fputs()`. Nedan följer ett exempel på hur man kan använda funktionen för att skriva ett felmeddelande:

```Swift
if num1 > num2 {
    fputs("Nummer 1 är större än nummer 2", stderr)
}
```

Detta kommer att skriva ut felet till standard error istället för standard output. Det är också möjligt att använda `print()`-funktionen och ange målet för utskriften som standard error genom att ange parametern `to:`:

```Swift
if num1 > 10 {
    print("Numret är större än 10", to: &stderr)
}
```

## Fördjupning

Att skriva till standard error är en bra teknik för att separera felmeddelanden från vanlig output. Det är också möjligt att använda `FileHandle`-klassen för att skriva till en specifik fil istället för standard error. Denna klass är användbar om man vill samla alla felmeddelanden i en särskild loggfil.

En annan fördel med att skriva till standard error är att utskriften inte buffras, vilket betyder att alla felmeddelanden kommer att visas direkt när de uppstår. Detta är mycket användbart vid felsökning och kan spara tid när man letar efter fel i koden.

## Se också

- [Swift dokumentation: Writing to Standard Error](https://developer.apple.com/documentation/swift/filehandle/1410773-init)
- [How to use fputs() for printing to standard error in Swift?](https://stackoverflow.com/questions/40559724/how-to-use-fputs-for-printing-to-standard-error-in-swift)