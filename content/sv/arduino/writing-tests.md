---
title:                "Arduino: Skriva tester"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-tests.md"
---

{{< edit_this_page >}}

##Varför
Att skriva tester är en viktig del av utvecklingen av Arduino-program. Det hjälper dig att hitta och korrigera fel tidigt i processen, vilket sparar tid och frustration på lång sikt.

##Så här
Att skriva tester för dina Arduino-program är en enkel process som kan hjälpa dig att bygga bättre och mer tillförlitliga projekt. Eftersom Arduino är baserat på C++ kan du använda de flesta testramverk som finns för detta språk, till exempel Google Test eller Catch.

Här är ett exempel på hur du kan skriva en enhetstest i Arduino med hjälp av Google Test:

```Arduino
#include <gtest/gtest.h>
#include "my_program.h"

TEST(MyProgramTest, Addition) {
    EXPECT_EQ(add(2, 3), 5);
}

TEST(MyProgramTest, Subtraction) {
    EXPECT_EQ(subtract(10, 5), 5);
}

TEST(MyProgramTest, Multiplication) {
    EXPECT_EQ(multiply(4, 5), 20);
}

TEST(MyProgramTest, Division) {
    EXPECT_EQ(divide(20, 5), 4);
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Resultatet av dessa enhetstester skulle vara följande:

```
[==========] Running 4 tests from 1 test case.
[----------] Global test environment set-up.
[----------] 4 tests from MyProgramTest
[ RUN      ] MyProgramTest.Addition
[       OK ] MyProgramTest.Addition (0 ms)
[ RUN      ] MyProgramTest.Subtraction
[       OK ] MyProgramTest.Subtraction (0 ms)
[ RUN      ] MyProgramTest.Multiplication
[       OK ] MyProgramTest.Multiplication (0 ms)
[ RUN      ] MyProgramTest.Division
[       OK ] MyProgramTest.Division (0 ms)
[----------] 4 tests from MyProgramTest (0 ms total)

[----------] Global test environment tear-down
[==========] 4 tests from 1 test case ran. (0 ms total)
[  PASSED  ] 4 tests
```

Som du kan se är det väldigt enkelt att skriva och köra enhetstester i Arduino med hjälp av Google Test.

##Djupdykning
Att skriva tester för dina Arduino-program är inte bara en bra utvecklingspraxis, det hjälper också till att förbättra kvaliteten och tillförlitligheten hos dina projekt. Genom att testa varje del av din kod kan du vara säker på att det fungerar som det ska och hitta eventuella fel eller buggar innan de blir ett större problem.

Förutom enhetstester kan du också skriva systemtester för att testa hela programmet. Dessa tester hjälper till att säkerställa att alla delar av din kod fungerar tillsammans som de ska och ger ett helhetsperspektiv på ditt projekt.

Se även
- [Arduino sketch testing](https://www.arduino.cc/en/Guide/UnitTesting)
- [Using Google Test for Arduino projects](https://libre.sciences.free.fr/ArduinoGoogleTest/)
- [Introduction to test-driven development for Arduino](https://www.wired.com/story/test-driven-development-easier-arduino-projects/)