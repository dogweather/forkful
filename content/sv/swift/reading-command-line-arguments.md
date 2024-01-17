---
title:                "Läsning av kommandoradsargument"
html_title:           "Swift: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När man skapar program är det vanligt att man ibland vill ha möjligheten att lägga till extra instruktioner när man kör programmet. Detta kan vara för att göra det mer flexibelt eller för att anpassa det till olika användningsfall. Det är här läsning av kommandoradsargument kommer in.

Programmerare utför läsning av kommandoradsargument för att få tillgång till extra information som användaren väljer att skicka med när de kör programmet. Det kan vara allt från enkel data som en sträng eller numeriskt värde till mer avancerade inställningar och flaggor.

## Så här:
```Swift
// Läsning av ett enkelt kommandoradsargument
let name = CommandLine.arguments[1] // name blir nu det andra argumentet som skickades med vid körning av programmet
print("Hej \(name)! Välkommen till programmet!")

// Mer komplex läsning med hjälp av optionals och flaggor
var message: String? = nil // Skapar en optional variabel som kan hålla värdet för flaggan
var debugMode = false // En bool för att hålla reda på om flaggan har använts när programmet körs

// Loopar igenom alla kommandoradsargument
for argument in CommandLine.arguments{
    switch argument{
        // Om flaggan "debug" finns med som argument sätts debugMode till true
        case "debug": debugMode = true
        // Om argumentet innehåller ordet "meddelande" läses det in som en sträng till variabeln message
        case let str where str.contains("meddelande"): message = argument
        // Ignorerar alla andra argument
        default: continue
    }
}

// Om debugMode är true skrivs ett extra meddelande ut
if debugMode{
    print("Kör programmet i debug-läge!")
}

// Om en sträng har skickats med som "meddelande" skrivs den ut
if let msg = message{
    print("Ditt meddelande: \(msg)")
} else {
    print("Inget meddelande skickades med.")
}
```

## Djupdykning:
Att läsa kommandoradsargument har varit en del av programmering sedan tidigt 70-tal då Unix-operativsystemet först introducerades. Det finns även alternativ till att läsa kommandoradsargument, som att använda miljövariabler istället.

I Swift finns möjligheten att använda CommandLine-structen för att få tillgång till kommandoradsargument. Den har också en inbyggd funktion för att validera och konvertera argumenten till rätt datatyper.

## Se även:
Läs mer om läsning av kommandoradsargument i Swift på [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/commandlinearguments).