---
title:                "Skriver till standardfel"
html_title:           "TypeScript: Skriver till standardfel"
simple_title:         "Skriver till standardfel"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standard error är en process där en programmerare kan direkt skriva ut felmeddelanden och error-messages till en särskild utgångskanal. Detta är en användbar teknik eftersom det separerar ut felmeddelanden från vanlig utdata och gör det enklare att identifiera och hantera fel i koden.

## Hur man gör:

```TypeScript
// Ett enkelt exempel på att skriva till standard error
process.stderr.write("Det här är ett error-meddelande!");
```

Detta kodblock kommer att skriva ut "Det här är ett error-meddelande!" till standard error-kanalen när koden körs. Det kan vara användbart när man vill markera ett specifikt fel eller varning i en logg.

## Djupdykning:

Historiskt sett har standard error använts tillsammans med standard output för att hantera fel och kommunikation med användaren. Alternativ till att skriva till standard error inkluderar att helt enkelt skriva ut till konsolen eller att använda ett specifikt fel- eller debug-meddelandesystem. 

I TypeScript är den vanligaste användningen av skrivning till standard error genom att använda "process.stderr.write()" funktionen. Denna funktion finns som standard i Node.js och behöver inte importeras.

## Se även:

- Node.js dokumentation för "process.stderr"
- "Writing to standard error" på Stack Overflow