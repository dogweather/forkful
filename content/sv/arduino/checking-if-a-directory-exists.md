---
title:                "Arduino: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

"## Varför"

Att kontrollera om en mapp existerar är ett viktigt steg i programmeringen av Arduino. Genom att vara säker på att en mapp finns kan du undvika felaktiga instruktioner och upprätthålla en smidig och effektiv kod.

"## Hur man gör det"

Kontrollera om en mapp existerar på Arduino är enkelt och kan göras med hjälp av funktionen "exists()" i File-klassen. Här är ett exempel på kod som visar hur man gör det:

```Arduino
#include <SPI.h>
#include <FS.h>
  
void setup() {
  Serial.begin(9600); // Öppna en kommunikationslinje med datorn
  while (!Serial) { } // Vänta tills kommunikationslinjen är öppen
  // Börja berätta för användaren vad som händer
  Serial.println("Kontrollera om mappen 'data' existerar...");
  // Anropa "exists()" metoden för att kontrollera om mappen existerar
  if(SPIFFS.exists("/data")){
    // Om mappen existerar, skriv ut ett meddelande
    Serial.println("Mappen 'data' existerar!");
  }
  else{
    // Om mappen inte existerar, skriv ut ett meddelande
    Serial.println("Kunde inte hitta mappen 'data'...");
  }
}

void loop() {
  // Inget behöver göras här, all kod körs i setup() funktionen
}
```

För att kunna köra denna kod behöver du installera SPIFFS-biblioteket på din Arduino. Mer information om hur du gör det hittar du här: [https://www.arduino.cc/en/Reference/FS](https://www.arduino.cc/en/Reference/FS)

## Djupdykning

När du använder "exists()" funktionen för att kontrollera om en mapp existerar, behöver du inkludera "/"-tecknet i början av mappens namn. Detta beror på att det är den fullständiga sökvägen som funktionen kontrollerar, inte enbart mappens namn.

En annan viktig aspekt att tänka på är att "exists()" funktionen bara kontrollerar om en mapp existerar och inte om den är tillgänglig för skrivning. Om du behöver kontrollera det senare, kan du använda "open()" funktionen som också ingår i File-klassen.

## Se även

För mer information om File-klassen och dess olika funktioner, rekommenderar vi att du tittar på Arduino officiella referensguide: [https://www.arduino.cc/reference/en/](https://www.arduino.cc/reference/en/)

Om du vill lära dig mer om hur du använder SPIFFS på Arduino, kan du hitta mer resurser och guider här: [https://www.arduino.cc/en/Reference/FS#installation](https://www.arduino.cc/en/Reference/FS#installation)