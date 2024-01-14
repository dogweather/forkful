---
title:    "Arduino: Jämföra två datum"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

##Varför
Att jämföra två datum är en vanlig uppgift i många projekt med Arduino. Det kan vara för att kolla när ett visst event ska ske, eller för att hålla koll på tiden mellan två händelser. Oavsett anledning, är det viktigt att veta hur man jämför datum på rätt sätt för att undvika fel i sitt program.

##Hur man gör
För att jämföra två datum på rätt sätt, bör du använda funktionen `compareDates()`. Den jämför två datum och returnerar ett heltal baserat på resultatet. Det här är ett enkelt exempel på hur man kan använda funktionen:

```Arduino
#include <DateTime.h>

void setup() {
  // Setup-kod här
}

void loop() {
  // Skapar två DateTime-objekt
  DateTime date1(2021, 4, 15);
  DateTime date2(2021, 4, 18);

  // Jämför de två datum
  int result = compareDates(date1, date2);

  // Skriver ut resultatet till seriell monitor
  Serial.begin(9600);
  Serial.println(result);

  // Väntar en sekund innan loopar om
  delay(1000);
}
```
I det här exemplet skapar vi två DateTime-objekt som representerar olika datum. Sedan använder vi `compareDates()`-funktionen för att jämföra dem. Resultatet, som returneras som ett heltal, skrivs ut till seriell monitor. Om date1 är före date2 kommer resultatet vara -1, om de är lika kommer resultatet vara 0, och om date1 är efter date2 kommer resultatet vara 1. 

Det är även möjligt att jämföra datum med tiden inkluderat. I så fall används `compare()`-funktionen istället, och den tar även hänsyn till tiden. Här är ett exempel på hur man kan använda den:

```Arduino
#include <DateTime.h>

void setup() {
  // Setup-kod här
}

void loop() {
  // Skapar två DateTime-objekt
  DateTime date1(2021, 4, 15, 14, 30, 0);
  DateTime date2(2021, 4, 15, 14, 31, 0);

  // Jämför de två datum och tider
  int result = compare(date1, date2);

  // Skriver ut resultatet till seriell monitor
  Serial.begin(9600);
  Serial.println(result);

  // Väntar en sekund innan loopar om
  delay(1000);
}
```

I det här exemplet jämför vi två datum och tider som har en minut emellan dem. Eftersom date1 är före date2 kommer resultatet vara -1.

##Djupdykning
Det finns flera saker att tänka på när man jämför datum i Arduino. En viktig aspekt är tidsformatet som används. I exemplet ovan använde vi `DateTime`-objekt, men det finns andra alternativ som du kan läsa mer om i dokumentationen för Arduino DateTime-biblioteket. Det är även viktigt att tänka på att jämföra datum konsekvent, till exempel genom att alltid använda samma format eller genom att konvertera till samma tidszon om det behövs.

Det finns också andra funktioner som kan vara användbara när man jämför datum, som `getDate()` för att få datum i formatet av en sträng eller `getDay()` för att få den specifika dagen i veckan för ett datum.

##Se även
Läs mer om `compareDates()` och andra DateTime-funktioner i Arduino DateTime-bibliotekets dokumentation på https://www.arduino.cc/reference/en/libraries/datetime/

Se även tutorialen om hur man använder DateTime i Arduino-projekt på https://www.arduino.cc/en/Tutorial/DateTimeComparison