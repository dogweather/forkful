---
title:                "Läsa in kommandoradsargument"
aliases:
- sv/swift/reading-command-line-arguments.md
date:                  2024-01-20T17:56:50.578773-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Kommandoradsargument är parametrar som skickas till ett program vid start. Programmerare använder dem för att styra programbeteendet utan att ändra koden.

## Hur man gör:
För att läsa kommandoradsargument i Swift, använder vi `CommandLine.arguments`. Här är ett litet exempel:

```Swift
print("Antal argument: \(CommandLine.arguments.count)")

for arg in CommandLine.arguments {
    print(arg)
}
```

Kör programmet så här i terminalen:
```bash
swift myprogram.swift arg1 arg2 arg3
```

Förväntad output:
```
Antal argument: 4
myprogram.swift
arg1
arg2
arg3
```

Observera att det första argumentet alltid är programmets sökväg.

## Fördjupning
Kommandoradsargument är en gammal tradition i programmering. De tillåter snabb interaktion med script och program, vilket är vanligt i Unix-liknande system. Alternativ till kommandoradsargument inkluderar konfigurationsfiler eller interaktiv inmatning under körning. I Swift sparas argumenten i en array - `CommandLine.arguments` - som är enkelt att iterera över. Det finns bibliotek som argparse i Python eller Rust's clap om du behöver mer avancerad funktionalitet för att hantera kommandoradsargument, men för många Swift-applikationer räcker grundfunktionaliteten.

## Se även
- [Swift Command Line Tool Tutorial](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial) - En guide för att skapa kommandoradsprogram i Swift.
- [Apple's Swift Documentation](https://developer.apple.com/documentation/swift) - Apples egna dokumentation av Swift, där `CommandLine` är en del av standardbiblioteket.
- [Swift ArgumentParser](https://github.com/apple/swift-argument-parser) - Ett Swift-bibliotek för att tolka kommandoradsargument, skapat av Apple.
