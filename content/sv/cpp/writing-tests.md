---
title:                "Skriva tester"
aliases:
- sv/cpp/writing-tests.md
date:                  2024-02-03T19:30:00.015896-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i C++ innebär att skapa små, självständiga program som automatiskt verifierar beteendet hos delar av din kodbas. Programmerare gör detta för att säkerställa att deras kod fungerar som förväntat, för att förhindra regressioner (dvs. nya ändringar som bryter befintlig funktionalitet) och för att underlätta underhållbara kodbasar över tid.

## Hur man gör:

### Använda Google Test Framework

Ett av de mest populära tredjepartsbiblioteken för att skriva tester i C++ är Google Test. Först behöver du installera Google Test och länka det med ditt projekt. När det är inställt kan du börja skriva testfall.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Spara koden i en fil och kompilera den med g++-kompilatorn, länkande Google Test-biblioteket. Om allt är inställt korrekt kommer körning av det resulterande exekverbart att köra testet, och om `add`-funktionen fungerar som förväntat kommer du att se något liknande:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test från TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test från TestSuiteName (0 ms totalt)

[==========] 1 test från 1 test suite kördes. (1 ms totalt)
[  PASSED  ] 1 test.
```

### Använda Catch2

Ett annat populärt testramverk för C++ är Catch2. Det har en enklare syntax och kräver oftast inte länkning mot ett bibliotek (endast header-fil). Här är ett exempel på hur man skriver ett enkelt test med Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // Detta talar om för Catch att tillhandahålla en main() - gör bara detta i en cpp-fil
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Heltal multipliceras", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Efter att ha kompilerat och kört detta test ger Catch2 tydlig utdata som indikerar om testet godkändes eller misslyckades, tillsammans med all information som behövs för att felsöka misslyckanden:

```
===============================================================================
Alla tester godkändes (1 påstående i 1 testfall)
```

Dessa exempel visar hur integrering av testramverk i din C++-utvecklingsprocess kan förbättra tillförlitligheten och underhållbarheten hos din kod avsevärt.
