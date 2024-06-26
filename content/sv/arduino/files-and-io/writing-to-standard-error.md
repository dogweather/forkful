---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:38.064364-07:00
description: "Hur man g\xF6r: Arduino skiljer inte nativt mellan standardutmatning\
  \ och standardfel som traditionella datorsystem g\xF6r. B\xE5de `Serial.print()`\
  \ och\u2026"
lastmod: '2024-03-13T22:44:38.184277-06:00'
model: gpt-4-0125-preview
summary: "Arduino skiljer inte nativt mellan standardutmatning och standardfel som\
  \ traditionella datorsystem g\xF6r."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
Arduino skiljer inte nativt mellan standardutmatning och standardfel som traditionella datorsystem gör. Både `Serial.print()` och `Serial.println()` metoder skriver till samma seriella utmatning, som vanligtvis visas i Arduino IDE:s serieövervakare. Vi kan dock emulera att skriva till stderr genom att specifikt formatera felmeddelanden eller dirigera dem till ett alternativt utmatningsmedel, såsom en fil på ett SD-kort eller över en nätverksanslutning.

För att emulera stderr kan du prefixa felmeddelanden med en tagg som "FEL:" för att differentiera dem i serieövervakaren:

```cpp
void setup() {
  Serial.begin(9600); // Initiera seriell kommunikation med 9600 baud
}

void loop() {
  int resultat = someFunction();
  if (resultat == -1) {
    // Emulerar stderr genom att prefixa felmeddelandet
    Serial.println("FEL: Funktionen misslyckades med att exekvera.");
  } else {
    Serial.println("Funktionen exekverades framgångsrikt.");
  }
  delay(1000); // Vänta en sekund innan loopen startas om
}

int someFunction() {
  // En dummy-funktion som returnerar -1 vid fel
  return -1;
}
```

Exempel på utmatning i Arduino IDE:s serieövervakare kan se ut så här:

```
FEL: Funktionen misslyckades med att exekvera.
```

För projekt som kräver ett mer sofistikerat tillvägagångssätt, inklusive att skriva till olika fysiska utmatningar, kan användning av tredjepartsbibliotek eller ytterligare hårdvara vara nödvändig. Till exempel kräver loggning av felmeddelanden till ett SD-kort biblioteket `SD`:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("FEL: Initialisering av SD-kort misslyckades!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("FEL: Funktionen misslyckades med att exekvera.");
    myFile.close(); // Se till att stänga filen för att spara innehållet
  } else {
    Serial.println("FEL: Öppning av error.log misslyckades!");
  }
}

void loop() {
  // Din huvudkod skulle gå här
}
```

Med detta tillvägagångssätt separerar du fysiskt normal programutmatning och felmeddelanden genom att rikta de senare till en `error.log` fil på ett SD-kort, vilket möjliggör post-mortem analyser utan att kladda till den primära utmatningskanalen.
