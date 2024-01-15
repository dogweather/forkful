---
title:                "Att kontrollera om en Mapp existerar"
html_title:           "Arduino: Att kontrollera om en Mapp existerar"
simple_title:         "Att kontrollera om en Mapp existerar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en katalog finns kan vara användbart för att undvika fel och effektivisera kod. Om du till exempel vill spara data till en viss katalog måste du först kontrollera att katalogen verkligen existerar innan du kan spara data till den. Detta säkerställer att din kod fungerar korrekt och undviker att skapa nya kataloger i onödan.

## Så här gör du

För att kontrollera om en katalog finns på din Arduino kan du använda funktionen "exists" från File-klassen. Detta gör det möjligt att söka efter en specifik katalog och få ett booleskt svar på om den finns eller inte.

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  // Initialiserar SD-kortet
  if(!SD.begin(10)) {
    Serial.println("Kunde inte hitta SD-kort.");
    return;
  }
  
  // Kontrollerar om katalogen "data" finns
  if(SD.exists("data")) {
    // Katalogen finns, fortsätt med kod
  } else {
    // Katalogen finns inte, gör något annat
  }

}

void loop() {

}
```

Om katalogen "data" finns kommer funktionen att returnera "true" och därmed kommer den första if-satsen att köras. Om katalogen inte finns kommer funktionen att returnera "false" och därmed kommer den andra if-satsen att köras. Det är också möjligt att använda "exists" för att kontrollera om en fil finns på samma sätt.

## Deep Dive

För att kunna använda funktionen "exists" måste du ha inkluderat File-biblioteket. Detta gör att du kan använda alla funktioner som finns tillgängliga för att hantera filer och kataloger på ditt SD-kort.

Det finns också möjlighet att använda mer avancerade metoder för att kontrollera om en katalog finns, som till exempel att utföra en sökning på hela SD-kortet för att hitta en specifik katalog. Detta kan vara användbart om du inte vet exakt vad katalogen heter eller om den kan placeras på olika platser på SD-kortet.

## Se även

Här är några användbara resurser för att lära dig mer om att hantera filer och kataloger på din Arduino:

- [File-klassen på Arduino referenssida](https://www.arduino.cc/en/Reference/FileExists)
- [SD-biblioteket på Arduino referenssida](https://www.arduino.cc/en/Reference/SD)
- [Guide för att använda SD-kort med Arduino](https://www.arduino.cc/en/Guide/ArduinoSD)