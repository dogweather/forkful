---
title:                "Jämföra två datum"
html_title:           "Arduino: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är användbart för att kunna hålla koll på tidsbaserade händelser, som till exempel när något ska aktiveras eller när ett event ska äga rum.

## Så här gör du
För att jämföra två datum i Arduino behöver du använda dig av funktionen "millis()", som returnerar antalet millisekunder sedan koden började köras. Här är ett exempel på hur du kan göra det:

```Arduino
// Definiera två variabler för dina datum
unsigned long tidpunkt1;
unsigned long tidpunkt2;

void setup() {
  // Sätt in början för datum 1
  tidpunkt1 = millis();
}

void loop() {
  // Uppdatera tiden för datum 2
  tidpunkt2 = millis();

  // Jämför de två tidpunkterna
  if (tidpunkt1 < tidpunkt2) {
    // Om tidpunkt1 är mindre än tidpunkt2, gör något
  } else if (tidpunkt1 > tidpunkt2) {
    // Om tidpunkt1 är större än tidpunkt2, gör något annat
  } else {
    // Om tidpunkterna är lika, gör något tredje
  }
}
```

Det här är en enkel jämförelse som bara använder sig av millisekunder. Du kan också använda dig av andra tidsenheter, som sekunder, minuter eller timmar, beroende på din specifika användning.

## Djupdykning
När du jämför två datum är det viktigt att tänka på att funktionen "millis()" kan återställas om enheten startas om eller om den når sitt maxvärde på ungefär 50 dagar. Detta kan påverka dina jämförelser och det är därför viktigt att ha det i åtanke när du använder denna metod.

En annan mer avancerad metod för att jämföra datum är att använda en extern modul, som en Real Time Clock (RTC). Detta är en enhet som kan hålla koll på tiden även när enheten stängs av eller återställs. Du kan ansluta en sådan modul till din Arduino och sedan använda dess funktioner för att jämföra datum.

## Se även
Här är några användbara länkar för att lära dig mer om att jämföra datum i Arduino:
- [Officiell Arduino guide för att arbeta med datum och tid](https://www.arduino.cc/en/Tutorial/BuiltInExamples/TimeSerial)
- [En artikel om att använda en RTC-modul med Arduino](https://www.instructables.com/id/Arduino-Real-Time-Clock-using-DS1307-RTC-module/)