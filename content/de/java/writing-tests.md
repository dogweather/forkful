---
title:    "Java: Tests schreiben"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-tests.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben von Tests ist ein wichtiger Bestandteil jeder guten Softwareentwicklungspraxis. Durch das Schreiben von Tests kann die Funktionalität und Fehlerfreiheit von Code sichergestellt und gleichzeitig die Wartbarkeit des Codes verbessert werden.

# Wie man Tests schreibt

Um Tests in Java zu schreiben, gibt es verschiedene Frameworks wie JUnit, TestNG oder Mockito. Hier ist ein Beispiel, wie man mit JUnit eine einfache Methode zum Addieren von zwei Zahlen testen kann:

```Java
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class CalculatorTest {

    @Test
    public void addNumbersTest() {
        // Arrange
        Calculator calc = new Calculator();
        
        // Act
        int result = calc.add(2, 3);
        
        // Assert
        assertEquals(5, result);
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```

Das `@Test`-Annotation weist JUnit an, diese Methode als Test auszuführen. Innerhalb der Methode wird der `Calculator` initialisiert und die Methode `add` aufgerufen. Anschließend wird mit der `assertEquals`-Methode überprüft, ob das erwartete Ergebnis (5) zurückgegeben wird.

# Tiefeneintauchen

Es gibt verschiedene Arten von Tests, die in der Java-Entwicklung verwendet werden können, wie z.B. Unit-Tests, Integrationstests oder End-to-End-Tests. Unit-Tests konzentrieren sich auf das Testen einer spezifischen Komponente oder Klasse, während Integrationstests die Interaktion zwischen verschiedenen Komponenten testen. End-to-End-Tests simulieren das Verhalten des Nutzers und testen die gesamte Anwendung.

Ein weiterer wichtiger Aspekt beim Schreiben von Tests ist die Testabdeckung. Diese gibt an, wie viel Prozent des Codes durch Tests abgedeckt sind. Eine hohe Testabdeckung bedeutet, dass der Code gründlich getestet wurde und somit weniger Fehler enthalten sollte.

# Siehe auch

- [JUnit Dokumentation](https://junit.org/junit5/docs/current/user-guide