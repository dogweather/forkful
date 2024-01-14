---
title:                "C++: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering, det hjälper till att säkerställa att koden fungerar som den ska innan den släpps ut i produktion. Det kan också bidra till att upptäcka och åtgärda eventuella fel eller buggar i koden tidigt i utvecklingsprocessen. Att använda tester kan också spara tid och resurser genom att förhindra att felaktig kod når användaren.

## Hur man gör

För att skriva tester i C++, behöver du en testerhetskod, som vanligtvis är en del av ett separat testbibliotek. Ett populärt testbibliotek för C++ är Google Test, som är en Open Source-lösning som används av många C++ utvecklare.

Först måste du installera och konfigurera Google Test för ditt projekt. Sedan kan du skriva tester för dina funktioner och klasser i en separat testfil. Här är ett enkelt exempel på hur en testfil kan se ut:

```C++
#include <gtest/gtest.h>

TEST(Exempeltest, Generelltest) {
    // förväntad utdata
    int expected = 10;
    // faktisk utdata
    int actual = minFunktion(5, 5);
    // verifiera att de är lika
    EXPECT_EQ(expected, actual);
}
```

När du kör testet kommer Google Test att rapportera om det lyckades eller misslyckades. Om det misslyckades kan du enkelt hitta och åtgärda felet i din funktion.

## Djupdykning

Att skriva effektiva tester är en färdighet som kräver övning. Det är viktigt att tänka på alla möjliga scenarier och fall som din kod kan stöta på och skriva tester för dem. Det är också viktigt att ha en välstrukturerad och lättläst testkod för att säkerställa att testerna är enkla att följa och förstå.

En annan viktig aspekt av tester är att undvika beroenden och isolera tester för att förhindra att ett fel i en del av koden påverkar resultatet av andra tester. Det är också bra att använda "mock" objekt för att simulera externa beroenden och undvika att faktiska anrop görs under testning.

Tänk på att skriva tester kontinuerligt och inte bara när koden är "klar". Att ha en god täckning av tester kommer att bidra till att upptäcka och lösa problem snabbare.

## Se även

- [Google Test dokumentation](https://github.com/google/googletest/blob/master/docs/googlemock.md)
- [Tutorial på hur man använder Google Test](https://www.softwaretestinghelp.com/cpp-unit-testing-with-google-test-tutorial/)
- [Skillnad mellan enhetstester, integrationstester och systemtester](https://www.softwaretestinghelp.com/unit-testing-vs-integration-testing-difference/)