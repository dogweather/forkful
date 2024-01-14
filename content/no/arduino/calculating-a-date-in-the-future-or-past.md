---
title:    "Arduino: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen ganger har lurt på hvilken dag det er om en uke eller om en måned, har du sannsynligvis vurdert å bruke en datokalkulator. Med Arduino kan du enkelt lage din egen datokalkulator, som kan være nyttig for å planlegge hendelser eller sette opp varsler.

## Hvordan

For å beregne en dato i fremtiden eller fortiden, trenger du følgende informasjon:
- Datoen du ønsker å beregne fra
- Antall dager som skal legges til eller trekkes fra
- Om du vil beregne en fremtidig eller fortidig dato

Først må du importere biblioteket "DateTime". Deretter må du opprette en variabel "startdate" som inneholder den aktuelle datoen. Deretter bruker du funksjonen "addDays()" for å legge til eller trekke fra det ønskede antallet dager. For å vise resultatet, bruker du funksjonen "toString()" og skriver ut variabelen "enddate" i en seriell monitor.

```Arduino
#include <DateTime.h> // Importerer biblioteket "DateTime"

DateTime startdate(2021, 8, 2); // Oppretter variabel for startdato

DateTime enddate = startdate.addDays(14); // Beregner en fremtidig dato ved å legge til 14 dager
Serial.println(enddate.toString()); // Skriver ut resultatet i seriell monitor
```

Output: 16-08-2021 (i "dd-mm-yyyy" format)

For å beregne en fortidig dato, bruker du samme kode, men erstatter "addDays()" med "subtractDays()".

```Arduino
DateTime enddate = startdate.subtractDays(30); // Beregner en fortidig dato ved å trekke fra 30 dager
Serial.println(enddate.toString()); // Skriver ut resultatet i seriell monitor
```

Output: 02-07-2021 (i "dd-mm-yyyy" format)

## Dyp Dykk

For å beregne en mer kompleks dato, som for eksempel neste mandag eller forrige tirsdag, kan du bruke funksjonen "dayOfWeek()", som returnerer en numerisk verdi (1-7) for hver ukedag. Deretter kan du bruke en switch statement for å legge til eller trekke fra dager basert på den ønskede ukedagen.

Du kan også beregne en dato ut fra et gitt antall uker, måneder eller år ved å bruke funksjonene "addWeeks()", "addMonths()" eller "addYears()".

## Se Også

- Dokumentasjon for DateTime biblioteket: https://playground.arduino.cc/code/datetime/
- Calculating Time and Date with Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/DateTimeCalculator
- Arduino Forum: https://forum.arduino.cc/