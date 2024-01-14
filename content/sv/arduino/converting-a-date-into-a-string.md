---
title:                "Arduino: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en praktisk funktion som kan hjälpa dig att visa datuminformation på en skärm eller använda det i en annan del av din kod. Till exempel kan du använda denna funktion för att visa arbetsordrar eller deadlines på en LCD-skärm.

## Så här gör du

För att konvertera ett datum till en sträng måste du först inkludera biblioteket "Time.h" och deklarera en instans av klassen "tmElements_t". Sedan kan du använda funktionen "makeTime" för att skapa en tid-stämpel med hjälp av år, månad, dag och tid. Slutligen kommer "strftime" att konvertera tid-stämpeln till en sträng enligt ett angivet format.

```arduino
#include <Time.h>

tmElements_t time; //deklarera instans av klassen
int year = 2021;
int month = 6;
int day = 15;
int hour = 14;
int minute = 30;
time_t timestamp = makeTime(0, minute, hour, day, month, year); //skapa tidstämpel
char dateString[20]; //skapa en array för strängen
strftime(dateString, 20, "%d-%m-%Y kl. %H:%M", timestamp); //konvertera till sträng
Serial.println(dateString); //skriv ut strängen på seriella monitorn
```

Output:
```
15-06-2021 kl. 14:30
```

## Deep Dive

När du använder funktionen "makeTime" är det viktigt att komma ihåg att det krävs en tidszonjustering. Du kan använda funktionen "setTime" för att justera tidszonen för att matcha din lokala tid. Dessutom kan du välja vilket format din sträng ska ha genom att ändra parametrarna i "strftime" funktionen.

Se till att kolla in dokumentationen för mer information och fler alternativ för att anpassa dina datumsträngar.

## Se även

- Dokumentation för Time.h biblioteket: https://www.arduino.cc/en/Reference/Time
- Guide för att ansluta en LCD-skärm till Arduino: https://www.circuitbasics.com/how-to-set-up-an-lcd-display-on-an-arduino/
- "strftime" dokumentation: https://www.cplusplus.com/reference/ctime/strftime/