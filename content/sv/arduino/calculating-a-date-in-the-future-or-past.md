---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Arduino: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av ett datum i framtiden eller förflutna är processen att hitta en specifik tidpunkt utifrån en annan genom att lägga till eller subtrahera dagar, veckor, månader eller år. Programmers gör detta för att följa händelselogger, beräkna dödsdatum eller för att navigera sekvenser av händelser i applikationer.

## Hur man gör:

Med Arduino kan du använder inbyggd `delay()` funktion för att skapa en arterföljd av händelser. Här är ett grundläggande kodexempel och dess utmatning:

```Arduino
void setup() {
  // börja serial kommunikation
  Serial.begin(9600);
}

void loop() {
  // skriv ut det aktuella datumet
  Serial.print("Nuvarande datum: ");
  Serial.println(millis()/86400000);
  // vänta i en vecka
  delay(604800000);
  // skriv ut det framtida datumet
  Serial.print("Datum om en vecka: ");
  Serial.println(millis()/86400000);
}
```

Utomatningen för denna kod kommer att skriva ut nuvarande datum, vänta i en vecka och sedan skriva ut framtida datum.

## Djup dykning:

Historiskt sett beräknades framtida och förflutet datum manuellt vilket var komplext och felbenäget. Med modern programmering kan detta bli mycket exakt och automatiserad. Alternativen till Arduino för datumberäkning inkluderer andra programmeringsspråk som Python, Java och C++ som har mer sofistikerade bibliotek för datum- och tidshantering. Detaljerna i datumberäkning med Arduino innefattar att använda millis() funktionen som returnerar antalet millisekunder sedan Arduino-styrenheten började, och sedan konvertera dessa millisekunder till dagar.

## Se också:

- Arduino Referens Dokumentation: [millis()](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- Förklaring av tids- och datumhantering i programmering: [Time and Date Handling in Programming](https://www.hanselman.com/blog/DateTimeProgrammingPitfalls.aspx)
- Python, Java och C++ datum- och tidshanteringsbibliotek:
   - Python: [Python datetime module](https://docs.python.org/3/library/datetime.html)
   - Java: [Java Time Package Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
   - C++: [C++ Chrono Library](https://en.cppreference.com/w/cpp/chrono)