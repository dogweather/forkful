---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben bedeutet automatisierte Programme zu erstellen, die deinen Code prüfen. Sie helfen Fehler zu vermeiden, geben Sicherheit bei Änderungen und erhöhen die Qualität deiner Software.

## How to:
Hier ist ein einfaches Beispiel mit `assert` in C. Es prüft die Funktion `addieren`, die zwei Zahlen summiert.

```C
#include <assert.h>

int addieren(int a, int b) {
    return a + b;
}

int main() {
    assert(addieren(2, 2) == 4);
    assert(addieren(-1, -1) == -2);
    // assert(addieren(2, 2) == 5); // Dies würde zu einem Fehler führen, weil 2 + 2 != 5

    return 0;
}
```
Wenn alles passt, gibt es keine Ausgabe. Fehler würden im Terminal so aussehen:

```
a.out: main.c:9: main: Assertion `addieren(2, 2) == 5' failed.
Aborted (core dumped)
```

## Deep Dive
Tests begannen in den 60er Jahren. Früher manuell, heute automatisiert. Alternativen zu `assert` in C sind Test-Frameworks wie Check, CMocka oder Unity. Sie bieten Features wie Test-Suiten, Mock-Objekte und detailliertere Ergebnisberichte.

Implementierungsdetails: `assert` ist nützlich für einfache Checks. Wichtig ist, dass Testfälle isoliert und unabhängig sind, um Wechselwirkungen und falsche Ergebnisse zu vermeiden.

## See Also
- "Test Driven Development" von Kent Beck (Erweiterter Leitfaden zur Testentwicklung)
- [Check](https://libcheck.github.io/check/): Ein Unit-Test-Framework für C
- [CMocka](https://cmocka.org): Einfaches Mocking und Unit-Testing für C
- [Unity Test API](http://www.throwtheswitch.org/unity): Für Test Driven Development in C