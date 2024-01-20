---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa kommandoradsargument innebär att hämta de indata som ges till ett program vid dess start från terminalen. Detta är användbart när programmerare vill styra beteendet hos ett program baserat på användarens input.

## Såhär:

Du kan få tillgång till kommandoradsargument i Swift via `CommandLine.arguments`, vilket är en lista av strängar. Här är ett enkelt exempel:

```Swift
// main.swift
for argument in CommandLine.arguments {
    print("argument: \(argument)")
}
```

Kör programmet med några argument:

```terminal
$ swift run programmet argument1 argument2 
```

Och här är möjlig output:

```terminal
argument: .build/debug/programmet
argument: argument1
argument: argument2
```

## Djupdykning

Historiskt sett har kommandoradsargument varit ett effektivt sätt för operativsystemet att skicka parametrar till program. Swift har gjort detta mycket enklare med `CommandLine.arguments`.

Men det finns alternativ. Du kan använda bibliotek som `Swift Argument Parser` om du vill ha mer avancerad funktionalitet. 

Vad gäller implementationen lagrar Swift alla dessa argument i en array som vi kan iterera igenom. Notera att den första strängen kommer alltid vara programmets sökväg.

## Se även

- [Swift Argument Parser](https://github.com/apple/swift-argument-parser): Ett kraftfullt, Swift-nativt bibliotek för kommandoradsparsering.
- [Swift Documentation](https://swift.org/documentation/): Apples officiella Swift-dokumentation.
- [Command Line Programming](https://www.objc.io/issues/16-command-line/command-line-programs/): En bra artikel om command line programming i allmänhet.