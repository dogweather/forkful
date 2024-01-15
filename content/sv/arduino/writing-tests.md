---
title:                "Skrivning av tester"
html_title:           "Arduino: Skrivning av tester"
simple_title:         "Skrivning av tester"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-tests.md"
---

{{< edit_this_page >}}

#Varför

Vi vet alla att programmering kan vara roligt och spännande, men det kan också vara en utmaning. Att skriva kod som fungerar är en sak, men att se till att koden fungerar korrekt och förväntat är en annan. Här kommer testning in i bilden - att skriva tester kan hjälpa dig att säkerställa att din kod fungerar som den ska och minska risken för buggar och fel.

#Hur man gör

Att skriva tester i Arduino kan vara enkelt med hjälp av testbiblioteket "ArduinoUnit". Här är en grundläggande kod som visar hur du kan skapa och köra ett enkelt test:

```
#include <ArduinoUnit.h>

// Testfunktion för att kontrollera om två tal är lika
unittest(equalNumbers) {
  int num1 = 5;
  int num2 = 5;
  assertEquals(num1, num2);
}

// Köra alla tester
unittest_main();
```

Genom att inkludera testbiblioteket och skriva tester i unittest(), kan du enkelt skapa en uppsättning tester som körs automatiskt varje gång du laddar upp koden till din Arduino.

#Djupdykning

Nu när du har en grundläggande förståelse för testning, låt oss titta på några viktiga koncept att överväga när du skriver tester för din Arduino-kod.

En viktig sak att tänka på är att dina tester bör vara självständiga. Det betyder att varje test bör kunna köras utan att vara beroende av andra tester. Om du till exempel har flera tester som är beroende av en viss hårdvarukomponent, se till att detta test körs först innan du kör de andra testen.

En annan viktig aspekt är att inkludera både positiva och negativa tester. Det betyder inte bara att testa när koden fungerar som den ska, utan också att testa när den inte fungerar som den ska. Det här kan hjälpa till att avslöja eventuella buggar och fel som kan uppstå.

När du skriver tester är det också viktigt att ha en tydlig och konsekvent namngivningsstruktur för dina tester. På så sätt kan du enkelt identifiera och åtgärda eventuella fel och felmeddelanden som uppstår.

#Se även

- ArduinoUnit biblioteket: https://github.com/mmurdoch/arduinounit
- "Test Driven Development" för Arduino: https://www.arduino.cc/reference/en/test-driven-development/
- "Writing Stable Arduino Code" guide: https://www.arduino.cc/en/Guide/StableReleaseTest