---
title:                "Kotlin: Tests schreiben"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie ein Softwareentwickler sind, haben Sie höchstwahrscheinlich schon von "Tests" oder "Testgetriebener Entwicklung" gehört. Aber warum sollte man überhaupt Zeit darauf verschwenden, Tests zu schreiben? Schließlich kann man doch einfach den Code ausführen und schauen, ob er funktioniert, oder?

Die Wahrheit ist, dass Tests ein unverzichtbares Werkzeug in der Welt der Softwareentwicklung sind. Sie ermöglichen es uns, sicherzustellen, dass unser Code so funktioniert, wie wir es erwarten, und helfen uns dabei, mögliche Fehlerquellen frühzeitig zu erkennen. Außerdem erleichtern sie die Zusammenarbeit im Team, da jeder Entwickler nachvollziehen kann, was der Code tun sollte, und bei einem Fehler schnell den Ursprung finden kann.

## Wie geht man vor

Tests können in Kotlin mit dem integrierten Test-Framework "JUnit" geschrieben werden. Um ein neues Testprojekt zu erstellen, können Sie einfach "gradle init" in Ihrem Terminal ausführen und das Projekt mit Java und Kotlin als Sprache auswählen. Dann können Sie die "build.gradle" Datei anpassen und JUnit als Dependency hinzufügen.

Um einen einfachen Testfall zu schreiben, können Sie eine Klasse erstellen, die mit "Test" annotiert ist, und eine Funktion mit der "Test" Annotation innerhalb dieser Klasse schreiben. Innerhalb dieser Funktion können Sie Assertions verwenden, um zu überprüfen, ob das erwartete Ergebnis erzielt wurde. Hier ist ein Beispiel:

```Kotlin
class MathTest {

    @Test
    fun `adding numbers`() {
        val result = add(2, 2)
        assertEquals(4, result)
    }
    
    fun add(a: Int, b: Int): Int {
        return a + b
    }
}
```

Wenn Sie den Test ausführen, sollte er erfolgreich sein. Wenn wir jedoch das Ergebnis von "assertEquals" ändern, zum Beispiel von "4" auf "5", wird der Test fehlschlagen und uns darauf aufmerksam machen, dass unser Code nicht das erwartete Ergebnis liefert.

## Tiefgreifende Analyse

Es gibt viele verschiedene Arten von Tests, die in Kotlin geschrieben werden können, wie Unit-Tests, Integrationstests und Akzeptanztests. Jede Art hat ihre eigene Bedeutung und hilft uns dabei, die Qualität unseres Codes zu verbessern.

Ein weiterer wichtiger Aspekt beim Testen ist die Codeabdeckung. Dies bezieht sich darauf, wie viel unseres Codes tatsächlich von unseren Tests abgedeckt wird. Eine hohe Codeabdeckung bedeutet, dass wir mehr Vertrauen in unseren Code haben können, da mehrere Szenarien abgedeckt werden.

Es ist auch wichtig zu beachten, dass Tests nicht nur beim Schreiben des Codes hilfreich sind, sondern auch bei zukünftigen Änderungen und Erweiterungen. Denn wenn wir neue Funktionen hinzufügen, können wir sicherstellen, dass diese nicht die bestehende Funktionalität beeinträchtigen, indem wir die vorher geschriebenen Tests erneut ausführen.

## Siehe auch

1. Offizielle JUnit-Dokumentation: https://junit.org/junit5/docs/current/user-guide/
2. Codeabdeckung in Kotlin mit Jacoco: https://www.jetbrains.com/help/idea/code-coverage.html
3. TDD - Testgetriebene Entwicklung: https://www.amazon.de/Test-Driven-Development-Klassiker-Softwareengineering/dp/3446204463/ref=sr_1_1?dchild=1&keywords=test+driven+development&qid=1619926466&sr=8-1