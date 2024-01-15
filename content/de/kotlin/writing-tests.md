---
title:                "Tests schreiben"
html_title:           "Kotlin: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Egal ob du ein erfahrener Entwickler oder gerade erst anfängst zu programmieren bist, das Schreiben von Tests ist ein wichtiger Teil des Entwicklungsprozesses. Tests helfen dabei, die Funktionalität deiner Anwendung zu überprüfen und Fehler frühzeitig zu erkennen. Sie stellen sicher, dass deine Code-Änderungen keine unerwarteten Auswirkungen haben und sorgen für eine stabilere Anwendung. Außerdem erleichtern sie das Debugging, da du durch die Tests bereits eine genaue Vorstellung hast, wo der Fehler liegen könnte.

## Wie es geht

Um Tests in Kotlin zu schreiben, benötigst du das Testframework "JUnit". Dieses kann einfach über die Build-Tool-Abhängigkeit deines Projekts hinzugefügt werden. 

```Kotlin
val junitVersion = "4.12"
dependencies {
    testImplementation("junit:junit:$junitVersion")
}
```

Als nächstes erstellst du eine Klasse für deinen Test. Diese muss mit dem Suffix "Test" enden und als Parameter eine Instanz von JUnit's "TestCase" Klasse enthalten. Anschließend kannst du in der Klasse Methoden für deine einzelnen Tests definieren, indem du das Annotation "@Test" verwendest.

```Kotlin
class CalculatorTest : TestCase() {

     @Test
     fun testAddition() {
        // Arrange
        val calculator = Calculator()
        
        // Act
        val result = calculator.add(3, 5)
        
        // Assert
        assertEquals(8, result)
    }
}
```

Hier haben wir ein Beispiel für einen Test der "add" Methode eines "Calculator" Objekts. In der "testAddition" Methode legen wir zuerst unser erwartetes Ergebnis fest. Anschließend führen wir die Addition mit dem Calculator aus und überprüfen durch die "assertEquals" Methode, ob das tatsächliche Ergebnis mit unserem erwarteten übereinstimmt. 

## Tiefere Einblicke

Neben dem grundlegenden Testen deiner Anwendung bietet JUnit auch die Möglichkeit, sogenannte "Assertions" zu verwenden. Diese ermöglichen es, gezielt Aussagen über das Verhalten deiner Anwendung zu treffen. Ein gutes Beispiel hierfür sind die "assertThrows" und "expect" Methoden, die es dir erlauben, zu testen, ob eine bestimmte Exception geworfen wird. 

Das Schreiben von Tests sollte ein integraler Bestandteil deines Entwicklungsprozesses sein. Es ist wichtig, dass Tests regelmäßig ausgeführt werden und auch immer aktualisiert werden, wenn sich der Code ändert. So kannst du sicherstellen, dass deine Anwendung immer den gewünschten Anforderungen entspricht.

## Siehe Auch

- [Kotlin Dokumentation](https://kotlinlang.org/docs/home.html)
- [JUnit User Guide](https://junit.org/junit4/users-guide)
- [Android Testing mit Kotlin](https://developer.android.com/training/testing/unit-testing/instrumented-unit-tests.html)