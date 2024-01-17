---
title:                "Skrivning till standardfel"
html_title:           "Swift: Skrivning till standardfel"
simple_title:         "Skrivning till standardfel"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skrivning till standardfel är en teknik som används av programmerare för att skicka felmeddelanden eller andra viktiga meddelanden till standardfelströmmen istället för standardutdataströmmen. Detta är användbart för att skilja dessa typer av meddelanden från vanlig programutdata och för att göra det möjligt för utvecklare att enkelt identifiera och hantera fel i sina program.

## Hur man:
Här är ett exempel på hur man skriver ett meddelande till standardfelströmmen i Swift:

```
Swift.print("Det här är ett felmeddelande till standardfelströmmen", to: &standardError)
```

Detta kommer att skriva ut meddelandet "Det här är ett felmeddelande till standardfelströmmen" till standardfelströmmen istället för till standardutdataströmmen.

## Djupdykning:
Historiskt sett användes skrivning till standardfelströmmen för att rapportera eventuella problem eller fel i programmet till användaren. Alternativt kan utvecklare använda en loggningsfunktion för att skicka meddelanden till en loggfil istället för standardfelströmmen.

I Swift finns det en inbyggd global variabel som heter "standardError" som används för att specificera standardfelströmmen. Men utvecklare kan också ha sitt eget anpassade felmeddelande-utmatningsobjekt och skicka meddelanden till det istället.

## Se även:
- [Swift Standard Library Documentation](https://developer.apple.com/documentation/swift)
- [Logging in Swift](https://www.raywenderlich.com/5373-logging-in-swift)