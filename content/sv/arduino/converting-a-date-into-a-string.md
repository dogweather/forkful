---
title:                "Omvandling av ett datum till en sträng"
html_title:           "Arduino: Omvandling av ett datum till en sträng"
simple_title:         "Omvandling av ett datum till en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad är det och varför?

Konvertering av ett datum till en sträng är en vanlig uppgift för programmerare. Det innebär att ta ett datum i numerisk form och göra om det till ett textformat, som en del av en större kod eller för att visa datumet till användaren. Ofta används det för att göra koden mer läsbar och för att ge kontext till tidsrelaterade händelser.

## Hur gör man?

Det finns olika sätt att konvertera ett datum till en sträng, men det mest vanliga sättet är att använda en funktion som `toString()`. Detta kräver att man först definierar ett datumobjekt och sedan använder `toString()` för att konvertera det till en sträng enligt det format man väljer.

```Arduino
#include <Time.h>

void setup() {
  Time now = Time.now(); // Definiera ett datumobjekt som motsvarar nuvarande tid
  String dateString = now.toString("yyyy-MM-dd"); // Konvertera datumet till en sträng med angivet format
  Serial.println(dateString);
}

void loop() {
  // Tom slinga
}
```

Output:
`2021-05-24`

Man kan också använda enbart funktionen `print()` för att skriva ut det konverterade datumet direkt till serienumret.

```Arduino
#include <Time.h>

void setup() {
  Time now = Time.now(); // Definiera ett datumobjekt som motsvarar nuvarande tid
  Serial.print(now.toString("yyyy-MM-dd"));
}

void loop() {
  // Tom slinga
}
```

Output:
`2021-05-24`

## Djupdykning

Att kunna konvertera ett datum till en sträng är en viktig del av programmering eftersom det gör koden mer läsbar och förståelig för både programmerare och användare. Det finns också andra sätt att göra samma sak, till exempel genom att använda strukturer som `DateTime` och `timelib.h` biblioteket.

## Se även

För mer information och exempel på hur man konverterar ett datum till en sträng, se följande länkar:

- [Time.h biblioteket](https://www.arduino.cc/en/reference/time)
- [DateTime biblioteket](https://www.arduino.cc/en/reference/datetime)
- [timelib.h biblioteket](https://github.com/PaulStoffregen/Time)