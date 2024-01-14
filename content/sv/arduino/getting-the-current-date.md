---
title:                "Arduino: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

Varför: Det finns många möjliga skäl att engagera sig i att få den aktuella datumet i en Arduino-programmering. Det kan vara användbart för tidsrelaterade uppgifter, som att skapa en tidsstämpel för sensoravläsningar, automatisera belysningssystem eller schemalägga uppgifter.

## Så här gör du

För att få den aktuella datumet i en Arduino-programmering, behöver vi använda oss av en inbyggd funktion som heter `now()`. Denna funktion returnerar antalet sekunder som har gått sedan början av Unix-epoken (1 januari 1970, 00:00:00 UTC). För att få datumet används därefter funktionen `day()`, `month()` och `year()`, som returnerar respektive dag, månad och år för den angivna tiden.

```Arduino
#include <TimeLib.h>

void setup(){
  Serial.begin(9600); //Starta serieporten för att kunna skriva ut datumet
  setTime(12, 34, 56, 1, 1, 1990); //Ange en tidpunkt att hämta datumet för
}

void loop(){
  time_t currentSeconds = now(); //Hämta aktuellt antal sekunder
  Serial.print("Aktuellt datum: ");
  Serial.print(day(currentSeconds)); //Skriv ut aktuell dag
  Serial.print("/");
  Serial.print(month(currentSeconds)); //Skriv ut aktuell månad
  Serial.print("/");
  Serial.println(year(currentSeconds)); //Skriv ut aktuellt år
  delay(1000); //Vänta en sekund innan nästa utskrift
}
```

Output:
```
Aktuellt datum: 1/1/1990
```

## Utforska djupare

Mer avancerade användningsområden för att få den aktuella datumet inkluderar att beräkna skillnaden mellan olika tidsstämplar, hantera olika tidszoner och utföra tidsrelaterade beräkningar. Ett annat sätt att få den aktuella datumet är att använda sig av en realtidsklocka som ansluts till Arduino, vilket ger en mer exakt tid utan att behöva synkronisera med en extern tidsserver.

## Se även

- [TimeLib.h bibliotek](https://www.arduino.cc/reference/en/libraries/timelib/)
- [Hitta tid och datum med Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Time)
- [Real time clock (RTC) modul för Arduino](https://www.arduino.cc/en/Main/ArduinoBoardProMini)