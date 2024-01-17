---
title:                "Tests schreiben"
html_title:           "Swift: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-tests.md"
---

{{< edit_this_page >}}

# Was & Warum?
Beim Programmieren geht es nicht nur darum, Code zu schreiben, sondern auch sicherzustellen, dass dieser Code auch funktioniert. Das Schreiben von Tests ermöglicht es Programmierern, Fehler in ihrem Code frühzeitig zu erkennen und somit die Qualität der Software zu verbessern.

# Wie man das macht:
```Swift
// Beispiel eines einfachen Tests
func testAddition() {
    let result = add(a: 2, b: 3)
    assert(result == 5)
}

// Beispiel einer fehlschlagenden Test
func testDivision() {
    let result = divide(a: 10, b: 0)
    assert(result == 5)
}

// Beispiel einer erfolgreichen Testausgabe
Test Suite 'All tests'
     Test SubSuite 'addition'
         Test Case '-[MyTests.MyTests testAddition]' passed

// Beispiel einer fehlgeschlagenen Testausgabe
Test Suite 'All tests'
     Test SubSuite 'division'
         Test Case '-[MyTests.MyTests testDivision]' failed (0.000 seconds)

```

# Tiefere Einblicke:
Das Konzept des Testens von Code ist nicht neu und wurde bereits in den 1950er Jahren eingeführt. Mittlerweile gibt es verschiedene Testarten, wie z.B. Unittests, Integrationstests und Akzeptanztests. Alternativen zum Schreiben von Tests sind beispielsweise die Verwendung von Debugging-Tools oder das manuelle Durchführen von Tests.

Bei der Implementation von Tests ist es wichtig, klar definierte Anforderungen an den Code zu haben und diese in den Tests zu berücksichtigen. Auch die Verwendung von Mock-Objekten kann hilfreich sein, um Abhängigkeiten zu anderen Teilen des Codes zu minimieren.

# Siehe auch:
Weitere Informationen und Beispiele zum Schreiben von Tests in Swift finden Sie in der offiziellen Dokumentation von Apple (https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/01-introduction.html).