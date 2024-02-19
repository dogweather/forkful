---
aliases:
- /de/cpp/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:07.863598-07:00
description: "Tests in C++ zu schreiben bedeutet, kleine, in sich geschlossene Programme\
  \ zu erstellen, die automatisch das Verhalten von Abschnitten Ihrer Codebasis\u2026"
lastmod: 2024-02-18 23:09:05.192733
model: gpt-4-0125-preview
summary: "Tests in C++ zu schreiben bedeutet, kleine, in sich geschlossene Programme\
  \ zu erstellen, die automatisch das Verhalten von Abschnitten Ihrer Codebasis\u2026"
title: Tests Schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Tests in C++ zu schreiben bedeutet, kleine, in sich geschlossene Programme zu erstellen, die automatisch das Verhalten von Abschnitten Ihrer Codebasis verifizieren. Programmierer tun dies, um sicherzustellen, dass ihr Code wie erwartet funktioniert, um Regressionen zu verhindern (d. h., neue Änderungen brechen bestehende Funktionalitäten), und um mit der Zeit wartbare Codebasen zu erleichtern.

## Wie:

### Verwendung des Google Test Frameworks

Eine der beliebtesten Dritt-Bibliotheken zum Schreiben von Tests in C++ ist Google Test. Zuerst müssen Sie Google Test installieren und es mit Ihrem Projekt verlinken. Sobald alles eingerichtet ist, können Sie mit dem Schreiben von Testfällen beginnen.

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

Speichern Sie den Code in einer Datei und kompilieren Sie ihn mit dem g++-Compiler, wobei Sie die Google Test-Bibliothek verlinken. Wenn alles korrekt eingerichtet ist, wird durch das Ausführen des resultierenden ausführbaren Programms der Test ausgeführt. Wenn die `add`-Funktion wie erwartet funktioniert, sehen Sie so etwas wie:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Verwendung von Catch2

Ein weiteres beliebtes Testframework für C++ ist Catch2. Es hat eine einfachere Syntax und erfordert in der Regel keine Verlinkung gegen eine Bibliothek (nur Header). Hier ist ein Beispiel, wie man einen einfachen Test mit Catch2 schreibt:

```cpp
#define CATCH_CONFIG_MAIN  // Dies sagt Catch, dass es eine main()-Funktion bereitstellen soll - tun Sie dies nur in einer cpp-Datei
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Ganzzahlen werden multipliziert", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Beim Kompilieren und Ausführen dieses Tests bietet Catch2 eine klare Ausgabe an, die anzeigt, ob der Test bestanden oder fehlgeschlagen ist, zusammen mit allen Informationen, die zum Debuggen von Fehlern benötigt werden:

```
===============================================================================
Alle Tests bestanden (1 Behauptung in 1 Testfall)
```

Diese Beispiele zeigen, wie die Integration von Test-Frameworks in Ihren C++-Entwicklungsworkflow die Zuverlässigkeit und Wartbarkeit Ihres Codes erheblich verbessern kann.
