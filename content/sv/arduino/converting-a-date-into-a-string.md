---
title:    "Arduino: Omvandla ett datum till en sträng"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför
Att konvertera ett datum till en sträng är en vanlig uppgift inom programmering och kan vara användbart när man vill presentera datumet på ett specifikt sätt, till exempel för utskrift på en skärm eller för lagring i en databas.

## Så här gör du
För att konvertera ett datum till en sträng i Arduino behöver vi först skapa en variabel av typen `Date` som innehåller det datum som vi vill konvertera. Sedan använder vi funktionen `strftime()` för att formatera datumet enligt våra önskemål och till slut skriva ut den till en variabel av typen `String`. Exempelkoden nedan visar hur detta kan göras för att formatera datumet som `ÅÅÅÅ-MM-DD`.

```Arduino
#include <Time.h> // inkluderar Time library

Date today = now(); // skapar en variabel för dagens datum
String dateString = ""; // variabel för att lagra strängen

dateString.reserve(10); // reserverar plats för 10 tecken, motsvarande längden på vår sträng

// formaterar datumet enligt önskat format och lagrar det i vår variabel
// strftime() tar som argument den sträng som ska formateras samt formatet för datumet
strftime(dateString, 10, "%Y-%m-%d", today);

// skriver ut strängen till seriell monitor eller annan lämplig utgång
Serial.println(dateString); 
```

Output:

`2021-03-29`

## Djupdykning
Förutom år, månad och dag kan `strftime()` även användas för att formatera andra delar av datumet, såsom veckodag eller klockslag. Det finns också möjlighet att ange olika format för det numeriska värdet på månaden, till exempel `"%m"` för "03" istället för `%b` för "mar". För att utforska alla möjligheter och hitta ett format som passar dina behov kan du läsa dokumentationen för Time library.

### Se även
- Time library dokumentation: https://www.arduino.cc/en/Reference/Time
- Utförlig guide för att formatera datum med `strftime()`: https://www.mkyong.com/c/how-to-use-strftime-function-to-format-current-date-and-time-in-c/
- För mer avancerade datumoperationer, se Date library: https://github.com/PaulStoffregen/TimeKeep