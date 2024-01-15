---
title:                "Tests schreiben"
html_title:           "Java: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Sie fragen sich vielleicht, warum es wichtig ist, Tests beim Programmieren zu schreiben. Nun, Tests helfen dabei, sicherzustellen, dass der Code wie erwartet funktioniert und verhindern mögliche Fehler oder Bugs. Sie sparen letztendlich Zeit und Mühe, da sie Probleme frühzeitig erkennen und beheben können.

## Wie

Um Tests in Java zu schreiben, können Sie die JUnit-Bibliothek verwenden. Zuerst müssen Sie sie jedoch in Ihr Projekt importieren. Legen Sie dann eine neue Klasse für Ihren Test an und importieren Sie die JUnit-Klassen. Erstellen Sie eine Methode mit der Annotation "@Test" und schreiben Sie Ihren Testcode innerhalb dieser Methode. Führen Sie schließlich Ihre Tests aus und überprüfen Sie die Ergebnisse im Konsolenfenster.

```Java
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TestCalculator {

    @Test
    public void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(10, calculator.add(5, 5));
    }
}
```

Die Ausgabe sollte die erfolgreiche Durchführung des Tests bestätigen.

## Tiefer eintauchen

Beim Schreiben von Tests ist es wichtig, verschiedene Szenarien abzudecken und randbedingungen zu berücksichtigen. Sie sollten auch sicherstellen, dass Ihre Tests unabhängig voneinander sind und nicht aufeinander aufbauen. Verwenden Sie außerdem aussagekräftige Testnamen, um die Lesbarkeit und Verständlichkeit zu verbessern.

Es ist auch hilfreich, regelmäßig Tests auszuführen und diese in Ihre Entwicklungsroutine einzubinden. Das macht es einfacher, Probleme frühzeitig zu erkennen und zu beheben.

## Siehe auch

- [JUnit Dokumentation](https://junit.org/junit5/docs/current/user-guide/)
- [Tutorial: Tests schreiben in Java mit JUnit](https://www.baeldung.com/junit-tests-runwith)
- [Best Practices für das Schreiben von Tests in Java](https://www.yegor256.com/2017/05/31/tests-best-practices.html)