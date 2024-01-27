---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben bedeutet, dass wir Code bereitstellen, der unseren Hauptcode automatisiert prüft. Wir tun dies, um Fehler frühzeitig zu entdecken, die Softwarequalität zu erhöhen und zukünftige Änderungen sicherer zu gestalten.

## How to:
Hier ist ein einfaches Beispiel für einen Unit-Test in C++ unter Nutzung des Google Test Frameworks:

```C++
#include <gtest/gtest.h>

int addieren(int a, int b) {
    return a + b;
}

TEST(RechenTest, AddierenTest) {
    EXPECT_EQ(3, addieren(1, 2));
    EXPECT_EQ(5, addieren(2, 3));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Laufen diese Tests, erhalten wir folgenden Output:

```
[==========] Running 2 tests from 1 test case.
[----------] Global test environment set-up.
[----------] 2 tests from RechenTest
[ RUN      ] RechenTest.AddierenTest
[       OK ] RechenTest.AddierenTest (0 ms)
[----------] 2 tests from RechenTest (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test case ran. (1 ms total)
[  PASSED  ] 2 tests.
```

## Deep Dive:
Unit-Tests sind seit den späten 90er Jahren populär geworden, besonders durch die Extreme Programming (XP) Bewegung. Alternativen zu Google Test sind unter anderem das Catch2 Framework und Boost.Test. Bei der Implementierung ist darauf zu achten, dass Tests isoliert und unabhängig von externen Zuständen sind, um zuverlässige und wiederholbare Ergebnisse zu gewährleisten.

## See Also:
Google Test (GitHub): https://github.com/google/googletest

Catch2: https://github.com/catchorg/Catch2

Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
