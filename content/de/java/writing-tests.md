---
title:                "Java: Tests schreiben"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Tests ist ein integraler Bestandteil der Java-Programmierung und sollte von jedem ernsthaften Entwickler praktiziert werden. Durch das Testen können Fehler und Probleme frühzeitig erkannt und behoben werden, was letztendlich zu einer besseren Codequalität und einer schnelleren Entwicklung führt.

## Wie man Tests schreibt

Die Erstellung von Tests in Java ist relativ einfach und kann in nur wenigen Schritten durchgeführt werden. Zunächst müssen Sie eine Testklasse erstellen und die `@Test`-Annotation von JUnit hinzufügen. Innerhalb der Testklasse können dann Testmethoden geschrieben werden. Hier ist ein Beispielcode:

```Java
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class CalculatorTest {
    @Test
    public void testAddition() {
        Calculator calc = new Calculator();
        int result = calc.add(3, 5);
        assertEquals(8, result);
    }
}
``` 

In diesem Beispiel wird die Testmethode `testAddition()` erstellt, um die `add()`-Methode der `Calculator`-Klasse zu testen. Mit der `assertEquals()`-Methode können wir überprüfen, ob das erwartete Ergebnis mit dem tatsächlichen Ergebnis übereinstimmt. 

## Deep Dive

Tests zu schreiben ist nicht nur eine Methode, um zu überprüfen, ob der Code wie erwartet funktioniert. Durch das Schreiben von Tests werden auch bestimmte Prinzipien der Softwareentwicklung gefördert, wie z.B. Modulareität und Wiederverwendbarkeit. Eine umfassendere Auseinandersetzung mit dem Thema Tests kann zu einer Verbesserung der allgemeinen Programmierfähigkeiten führen.

## Siehe auch

- [JUnit Documentation](https://junit.org/junit5/docs/current/user-guide/)
- [Writing Tests in Java - Tutorial](https://www.baeldung.com/java-junit-tests)
- [The Art of Writing Good Code](https://medium.com/better-programming/the-art-of-writing-good-code-6ef0f603fb98?source=collection_home---4------8-----------------------&gi=e0f288702570)