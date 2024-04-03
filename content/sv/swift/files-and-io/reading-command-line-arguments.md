---
date: 2024-01-20 17:56:50.578773-07:00
description: "Hur man g\xF6r: F\xF6r att l\xE4sa kommandoradsargument i Swift, anv\xE4\
  nder vi `CommandLine.arguments`. H\xE4r \xE4r ett litet exempel."
lastmod: '2024-03-13T22:44:38.266194-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att l\xE4sa kommandoradsargument i Swift, anv\xE4nder vi `CommandLine.arguments`."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

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
