---
title:                "Arduino: Skapa tester"
simple_title:         "Skapa tester"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-tests.md"
---

{{< edit_this_page >}}

##Varför skriva tester för Arduino-programmering

När man programmerar för Arduino kan det vara frestande att hoppa över testningen och direkt gå till att implementera koden på en fysisk enhet. Men att skriva tester för din Arduino-kod kan spara dig mycket tid och frustration i det långa loppet.

##Så här skriver du tester för Arduino-kod

För att skriva tester för Arduino-kod behöver du en testramverk som stödjer Arduino-bibliotek. Ett exempel på ett sådant ramverk är UnitTest++, som är gratis och öppen källkod.

Efter att du har installerat och konfigurerat testramverket kan du skriva dina tester i samma fil som din kod eller i separata filer. Här är ett exempel på en testklass som testar en funktion som adderar två tal:

```Arduino
#include <Arduino.h>
#include "UnitTest++.h" //inkludera testramverket

void addNumbers(int num1, int num2) { //testfunktion
  return num1 + num2;
}

TEST(AddNumbersTest) { //testklass
  //Arrangera
  int expected = 5;
  int actual = 5;
  
  //Utför
  actual = addNumbers(2, 3);
  
  //Asserta
  CHECK_EQUAL(expected, actual);
}

int main() {
  return UnitTest::RunAllTests(); //kör alla tester
}
```

Om testet passerar så kommer du se följande output i din seriella monitor:

```
Success: 1 tests passed.
```

##Djupdykning i att skriva tester för Arduino

Att skriva tester för Arduino-kod ger dig flera fördelar. Det hjälper till att felsöka dina program och hitta buggar innan du implementerar koden på en fysisk enhet. Det kan även hjälpa dig att förstå din kod bättre och underlätta vid vidare utveckling.

När du skriver tester för din kod bör du tänka på att testa alla möjliga scenarier och felkoder för att säkerställa att din kod fungerar som den ska. Det är även bra att återanvända tester när du uppdaterar din kod för att se till att inga nya buggar har introducerats.

##Se även

- [Unit testing in Arduino](https://www.arduino.cc/en/Guide/UnitTesting)
- [UnitTest++](https://github.com/unittest-cpp/unittest-cpp) testramverk för Arduino
- [Arduino: Getting Started with Testing](https://learn.adafruit.com/arduino-testing/overview) tutorial om testning för Arduino