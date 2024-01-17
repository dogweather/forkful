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

## Was ist das und Warum?
Tests schreiben ist eine wichtige und unverzichtbare Praxis im Programmieren. Dabei werden spezielle Codeabschnitte geschrieben, um zu überprüfen, ob die Funktionalität des Programms ordnungsgemäß funktioniert. Programmierer nutzen Tests, um sicherzustellen, dass ihr Code zuverlässig und fehlerfrei ist. Dies spart Zeit und Mühe bei der Fehlerbehebung und gewährleistet eine bessere Qualität des Codes.

## Wie geht's?
Die grundlegende Syntax zum Schreiben von Tests in Java ist folgende:

```Java
// Importieren der notwendigen Java-Klassen
import org.junit.*; 

// Erstellen einer Testklasse
public class MeinTest { 
  
  // Definieren von Testmethoden mit der Annotation "@Test"
  // Hier ein Beispiel
  @Test
  public void testAddition() { 
    // Initialisieren der Werte für den Test
    int a = 5; 
    int b = 3; 

    // Ausführen der zu testenden Funktion
    int ergebnis = a + b; 

    // Überprüfen, ob das Ergebnis korrekt ist
    Assert.assertEquals(8, ergebnis); 
  } 
} 
```
Der Codeblock beginnt mit dem Importieren der JUnit-Klasse, die für das Erstellen von Tests verwendet wird. Anschließend wird eine Testklasse erstellt, in der alle Testmethoden enthalten sind. Diese sind mit der Annotation "@Test" gekennzeichnet. Im Beispiel wird die Methode "testAddition" erstellt, welche die Addition zweier Zahlen überprüft. Durch die Verwendung von "Assert.assertEquals" wird geprüft, ob das Ergebnis der Addition dem erwarteten Ergebnis entspricht.

## Tiefere Einblicke
Die Praxis des Testens entstand in den 60er Jahren mit der Entwicklung von Software-Tests für den NASA-Großrechner. Seitdem hat sie sich zu einem wichtigen Bestandteil der Software-Entwicklung entwickelt. Es gibt auch alternative Ansätze zum Schreiben von Tests, wie zum Beispiel das Test-Driven Development (TDD), bei dem Tests vor dem eigentlichen Code geschrieben werden.

Es gibt auch spezielle Frameworks wie JUnit, TestNG oder Mockito, die die Erstellung von Tests erleichtern und erweiterte Funktionen bieten. Es ist wichtig, bei der Erstellung von Tests eine gute Testabdeckung zu erreichen, um sicherzustellen, dass alle möglichen Fälle getestet werden und der Code zuverlässig ist.

## Siehe auch
- [Offizielle JUnit-Dokumentation (auf Englisch)](https://junit.org/junit5/docs/current/user-guide/)
- [Informationen über den Einsatz von JUnit in der Java-Entwicklung](https://www.codecademy.com/articles/using-junit-with-java)
- [Test-Driven Development (TDD) erklärt (auf Deutsch)](https://entwickler.de/online/agile/test-driven-development-tdd-was-ist-das-579460351.html)