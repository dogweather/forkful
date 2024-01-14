---
title:                "Swift: Läsa in kommandoradsargument"
simple_title:         "Läsa in kommandoradsargument"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Varför##
I Swift-programmering är det vanligt att ha en dialog och samverkan med användaren. Att läsa kommandoradsargument kan ge programmet möjlighet att ta emot inmatning eller påverkas av användarens val. Det här blogginlägget kommer att utforska hur man läser kommandoradsargument i Swift.

##Så här gör du##
För att läsa kommandoradsargument i Swift behöver du först importera Foundation-modulen. Sedan kan du använda CommandLine-objektet för att få åtkomst till de olika argumenten som skickas till programmet.

```Swift
import Foundation

// Hämtar argumenten som en array av strängar från kommandoraden
let arguments = CommandLine.arguments

// Skriver ut de olika argumenten på en ny rad
for argument in arguments {
    print(argument)
}
```

Om vi till exempel kör programmet med följande kommandoradsargument: `swift read-arguments.swift argument1 argument2` så kommer utmatningen att se ut så här:

```
argument1
argument2
```

##Djupdykning##
När du läser kommandoradsargument kan du också ange ett index för att hämta ett specifikt argument. Om du till exempel bara vill hämta det andra argumentet som skickas in kan du använda syntaxen `arguments[1]` eftersom index börjar med 0.

```Swift
import Foundation

let arguments = CommandLine.arguments

// Hämtar andra argumentet som skickas in
let argument2 = arguments[1]

print(argument2)
```

Om vi nu kör programmet med samma kommandoradsargument som tidigare kommer utmatningen bara att visa `argument2`.

##Se även##
- [Beskrivning av CommandLine-objektet i Swift](https://developer.apple.com/documentation/foundation/commandline)
- [Mer information om Swift-programmering](https://developer.apple.com/swift/)