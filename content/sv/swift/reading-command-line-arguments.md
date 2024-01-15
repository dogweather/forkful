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

## Varför

Att kunna läsa kommandoradsargument är en grundläggande färdighet inom programmering, och är också mycket användbart när du utvecklar program som körs från kommandoraden. 

## Hur man gör

Det finns flera sätt att läsa kommandoradsargument i Swift, men här är ett enkelt exempel som du kan använda i dina projekt.

Först måste du importera Foundation framework i din Swift-fil:

```Swift 
import Foundation
```

Sedan kan du använda funktionen CommandLine.arguments för att få en array av alla kommandoradsargument som tillhandahålls vid körning av ditt program:

```Swift 
let arguments = CommandLine.arguments
```

Nu kan du använda dessa argument för att utföra önskade åtgärder inom ditt program.

Till exempel, om du vill skriva ut alla kommandoradsargument, kan du använda en enkel for-loop:

```Swift
for argument in arguments {
    print(argument)
}
```

Detta kommer att ge följande utmatning när du kör ditt program från kommandoraden tillsammans med några argument:

```
$ swift myProgram.swift argument1 argument2 argument3
argument1
argument2
argument3
```

Det är viktigt att notera att det första argumentet i arrayen av kommandoradsargument alltid är namnet på programmets körbara fil. Så om du till exempel kör ditt program med kommandot "swift myProgram.swift", kommer arrayen CommandLine.arguments att innehålla ["myProgram.swift"] som första argument.

## Djupdykning

Nu när vi har sett ett enkelt exempel på hur man kan läsa kommandoradsargument, låt oss titta lite djupare in på detta ämne.

För det första kan du använda funktionen CommandLine.argc för att få antalet kommandoradsargument som tillhandahålls vid körning av ditt program. Detta kan vara användbart om du vill göra något baserat på antalet argument som matas in.

En annan bra funktion att känna till är CommandLine.arguments.suffix(n), som returnerar en del av arrayen CommandLine.arguments från och med det n:te elementet till slutet. Detta kan vara användbart om du bara vill ha vissa argument och inte alla.

När det gäller att hantera argument som är filnamn eller sökvägar, är det viktigt att ta hänsyn till eventuella mellanslag eller specialtecken som kan finnas i dem. För att undvika problem med detta bör du använda funktionen stringByAddingPercentEncodingWithAllowedCharacters() från Foundation framework för att korrekt koda argumenten innan du använder dem i ditt program.

## Se även

Här är några länkar till andra resurser som kan vara användbara när du arbetar med kommandoradsargument i Swift:

- [Apples officiella dokumentation för Foundation framework](https://developer.apple.com/documentation/foundation)
- [Exempel på att läsa kommandoradsargument i Swift](https://www.avanderlee.com/swift/command-line-arguments/)
- [En video om att arbeta med kommandoradsargument i Swift](https://www.youtube.com/watch?v=_gHGYa9qbss)

Det är allt för denna artikel. Hoppas det hjälpte dig att förstå hur man kan läsa kommandoradsargument i Swift. Lycka till i dina framtida projekt!