---
title:    "Arduino: Kontrollera om en katalog finns"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Varför: 

Att kontrollera om en mapp finns är en viktig del av Arduino-programmering. Genom att känna till hur man gör detta kan du säkerställa att din kod fungerar korrekt och undvika felmeddelanden. Det är också viktigt för att kunna hantera lagring och åtkomst av data på ett effektivt sätt.

## Hur man gör det: 

Det första steget för att kontrollera om en mapp finns är att använda funktionen `exists()` från biblioteket `SD.h`. Här är ett exempel på hur man kan använda denna funktion:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  // Anslut SD-kortet till modulen
  pinMode(10, OUTPUT);
  if (!SD.begin(10)) {
  // Om SD-kortet inte kan hittas, skriv ut felmeddelande
    Serial.println("Kan inte ansluta till SD-kortet!");
    return;
  }
  // Definiera sökvägen till mappen som ska kontrolleras
  String mapp_sokvag = "/EXEMPEL/";
  // Kontrollera om mappen finns
  if (SD.exists(mapp_sokvag)) {
    // Skriv ut meddelande om mappen finns
    Serial.println("Mappen finns!");
  } else {
    // Skriv ut meddelande om mappen inte finns
    Serial.println("Mappen finns inte!");
  }
}

void loop() {
  // Kör inte någon annan kod i loopen för att undvika att överbelasta SD-kortet
}
```

Detta kodexempel förutsätter att du har anslutit ett SD-kort till modulen och definierat en sökväg till en mapp som heter "EXEMPEL". Om mappen finns kommer du att se ett meddelande i serieporten som bekräftar detta.

## Fördjupa dig: 

För att förbättra din kod och bli mer flexibel kan du också lägga till ett villkor för att skapa mappen om den inte redan finns. Detta kan göras med hjälp av `mkdir()`-funktionen från samma bibliotek. Om mappen inte finns kommer den att skapas och om den redan finns kommer ingenting att hända. Detta kan vara användbart om du behöver ha en specifik mappstruktur för dina data.

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  // Anslut SD-kortet till modulen
  pinMode(10, OUTPUT);
  if (!SD.begin(10)) {
  // Om SD-kortet inte kan hittas, skriv ut felmeddelande
    Serial.println("Kan inte ansluta till SD-kortet!");
    return;
  }
  // Definiera sökvägen till mappen som ska kontrolleras
  String mapp_sokvag = "/EXEMPEL/";
  // Kontrollera om mappen finns
  if (SD.exists(mapp_sokvag)) {
    // Skriv ut meddelande om mappen finns
    Serial.println("Mappen finns!");
  } else {
    // Skapa mappen
    if (SD.mkdir(mapp_sokvag)) {
      // Meddela om att mappen har skapats
      Serial.println("Mappen har skapats!");
    } else {
      // Meddela om att mappen inte kunde skapas
      Serial.println("Mappen kunde inte skapas!");
    }
  }
}

void loop() {
  // Kör inte någon annan kod i loopen för att undvika att överbelasta SD-kortet
}
```

Med detta fördjupade exempel kan du hantera både mapp- och filskapelse på ett enkelt sätt.

## Se även: 

Här är några andra användbara resurser för att lära dig mer om Arduino-programmering och arbete med SD-kort:

- [Arduino Bibliotek Guide](https://www.arduino.cc/en/Reference/LibraryManager)
- [SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Guide för att Arbeta med SD-kort och Arduino](https://www.arduino.cc/en/Tutorial/Files)

Lycka till med dina projekt som involverar Arduino och SD-kort!