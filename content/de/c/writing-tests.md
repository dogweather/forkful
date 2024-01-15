---
title:                "Tests schreiben"
html_title:           "C: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-tests.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man Tests schreiben? Nun, Tests sind ein wichtiger Bestandteil des Software-Entwicklungsprozesses. Sie helfen dabei, Fehler frühzeitig zu erkennen und sicherzustellen, dass der Code zuverlässig funktioniert. Das spart Zeit und Nerven und erhöht die Qualität der Software.

# Wie geht's

## Testfälle definieren

Bevor Sie mit dem Schreiben von Tests beginnen, sollten Sie überlegen, welche Funktionen Ihre Software haben soll und welche Randfälle möglicherweise auftreten können. Definieren Sie dann die Testfälle entsprechend und schreiben Sie sie auf.

```C
// Beispiel für die Definition von Testfällen
// Hier testen wir eine Funktion, die zwei Zahlen addiert
// Es gibt drei Testfälle: normale Addition, Überlauf und Null-Handling

// Normale Addition
assert(add(5, 7) == 12);

// Überlauf-Test
assert(add(INT_MAX, 3) == INT_MIN + 2);

// Null-Handling-Test
assert(add(0, 0) == 0);
```

## Implementierung der Tests

Nachdem Sie Ihre Testfälle definiert haben, ist es Zeit, die Tests zu implementieren. Dies kann mit Hilfe von Test-Frameworks wie z.B. CUnit oder Check erfolgen. Diese bieten bereits Funktionen zum Testen und Auswerten von Aussagen.

```C
// Beispiel für die Implementierung der Tests mit CUnit
#include <CUnit/Basic.h>

// Diese Funktion wird vor jedem Testfall ausgeführt
void setUp(void) {}

// Diese Funktion wird nach jedem Testfall ausgeführt
void tearDown(void) {}

// Beispiel für einen Testfall mit CUnit
void add_test(void) {
   CU_ASSERT(add(5, 7) == 12);
   CU_ASSERT(add(INT_MAX, 3) == INT_MIN + 2);
   CU_ASSERT(add(0, 0) == 0);
}
```

## Ausführen der Tests

Sobald die Tests implementiert sind, können Sie sie ausführen, um sicherzustellen, dass alle Testfälle erfolgreich durchlaufen werden. Achten Sie auf Fehlermeldungen und passen Sie gegebenenfalls Ihren Code an.

```C
// Beispiel für das Ausführen der Tests mit CUnit
int main() {
    // Initialisieren des Test-Frameworks
    CU_initialize_registry();

    // Einrichten der Test-Suite und Hinzufügen der Testfälle
    CU_pSuite suite = CU_add_suite("Suite Name", setUp, tearDown);
    CU_add_test(suite, "Testfall Name", add_test);

    // Ausführen der Tests
    CU_basic_run_suite(suite);

    // Aufräumen und Ergebnis ausgeben
    CU_cleanup_registry();
    return 0;
}
```

# Tiefer Einblick

## Warum Testen wichtig ist

Das Schreiben von Tests ist wichtig, weil es Ihnen hilft, potenzielle Fehlerquellen in Ihrem Code zu erkennen und zu beseitigen. Durch regelmäßiges Testen können Sie sicherstellen, dass Ihre Software zuverlässig funktioniert und keine unerwarteten Fehler verursacht.

## Best Practices

Beim Schreiben von Tests gibt es einige bewährte Methoden, die Ihnen helfen, effektive und effiziente Tests zu schreiben. Dazu gehören das Definieren von Testfällen, das Trennen von Test- und Produktionscode, das Verwenden von Test-Frameworks und das regelmäßige Ausführen von Tests.

# Siehe auch

- [CUnit Dokumentation](http://cunit.sourceforge.net/)
- [Check Dokumentation](https://libcheck.github.io/check/)
- [Test Driven Development in C](https://dzone.com/articles/test-driven-development-in-c)