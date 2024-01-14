---
title:    "Arduino: Jämföra två datum"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra datum är en vanlig uppgift inom programmering, oavsett om du utvecklar en app, en hemsida eller ett Arduino-projekt. Det kan vara användbart för att kontrollera tidsintervall, schemaläggning eller för att jämföra datum för olika händelser. I denna blogginlägg kommer jag att visa hur du kan jämföra två datum med hjälp av Arduino-programmering.

## Hur man gör det

För att jämföra två datum i en Arduino-sketch, behöver du först skapa två variabler som representerar de två datumen du vill jämföra. Dessa variabler måste vara av typen "Date" och deklareras med hjälp av funktionen "Date" som finns inbyggd i Arduino-realtime clock library.

```Arduino
/* Import av bibliotek */
#include <RTClib.h>

/* Deklarera variabler för två datum */
DateTime t1; // Första datum
DateTime t2; // Andra datum
```

När variablerna är deklarerade, kan du fylla i dem med önskade datum genom att använda funktionen "now()" som hämtar tiden från den inbyggda klockan på din Arduino.

```Arduino
/* Hämta aktuell tid för första datumet */
t1 = now(); 

/* Ange ett annat datum för det andra datumet */
t2 = DateTime(2020, 9, 10, 12, 30, 0); 
```

För att jämföra de två datumen kan du använda funktionen "compare()" som returnerar ett heltal som indikerar vilket datum som är tidigare, eller om de är lika.

```Arduino
/* Jämför två datum och spara resultatet i en variabel */
int resultat = t1.compare(t2); 

if (resultat == -1) { 
    /* Om resultatet är -1, är t1 tidigare än t2 */
    Serial.println("t1 är tidigare än t2"); 
}
else if (resultat == 1) { 
    /* Om resultatet är 1, är t2 tidigare än t1 */
    Serial.println("t2 är tidigare än t1"); 
}
else { 
    /* Om resultatet är 0, är t1 och t2 lika */
    Serial.println("t1 och t2 är lika"); 
}
```

Beroende på vad du vill uppnå med jämförelsen kan du också använda funktionerna "isBefore()" och "isAfter()" för att endast få true eller false-svar.

## Djupdykning

Det finns olika sätt att jämföra datum i Arduino beroende på vilka bibliotek och funktioner du har tillgängliga. Du kan till exempel använda biblioteket "TimeLib.h" och dess funktioner "now()" och "makeTime()" för att skapa Date-objekt. Dessutom finns det olika sätt att formatera datumen, antingen som en sträng eller genom att hämta specifika komponenter såsom år, månad, dag osv. Det är viktigt att välja det bästa sättet för ditt specifika projekt och att förstå hur datumen ska användas.

## Se även

- Enkel guide för att skriva Arduino-sketcher: https://makecode.adafruit.com/
- En översikt av tid och datum i Arduino: https://www.arduino.cc/reference/en/libraries/
- Mer information om Arduino-realtime clock library: https://github.com/adafruit/RTClib