---
title:    "Swift: Läsning av kommandoradsargument"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument är en viktig färdighet i Swift-programmering, särskilt för de som vill skapa terminalapplikationer. Genom att lära sig detta kan man ta emot input från användaren och bearbeta det på ett effektivt sätt.

## Så här gör du

Först måste vi importera Foundation-biblioteket för att få tillgång till CommandLine-klassen. Sedan kan vi använda CommandLine.arguments för att få en array av alla argument som skickades in när programmet startades. Här är ett exempel:

```Swift
import Foundation

// Hämta alla argument
let args = CommandLine.arguments

// Skriv ut alla argument
print("De angivna argumenten är: \(args)")
```

Kör detta program i terminalen med några olika argument och se hur det fungerar!

## Djupdykning

CommandLine-klassen är mycket kraftfull och ger oss olika möjligheter att hantera kommandoradsargument. Vi kan också använda följande metoder:

- `CommandLine.arguments.first` för att hämta det första argumentet i arrayen, vilket oftast är namnet på programmet
- `CommandLine.arguments.last` för att hämta det sista argumentet
- `CommandLine.arguments.dropFirst()` för att få en array med alla argument utom det första
- `CommandLine.arguments.dropLast()` för att få en array med alla argument utom det sista

Vi kan också använda `CommandLine.arguments.contains()` för att kontrollera om ett visst argument finns med eller inte. Det finns också en möjlighet att definiera egna flaggor och parametrar som kan läsas in med hjälp av CommandLine-klassen.

## Se även

- [CommandLine documentation](https://developer.apple.com/documentation/foundation/commandline)
- [How to Read Command Line Arguments in Swift](https://www.avanderlee.com/swift/read-command-line-arguments/)

Tack för att du läste denna guide om hur man läser in kommandoradsargument i Swift. Vi hoppas att du nu känner dig bekväm med att hantera och bearbeta input från användare i dina terminalapplikationer. Lycka till med din Swift-programmering!