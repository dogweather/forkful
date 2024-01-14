---
title:                "Java: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-tests.md"
---

{{< edit_this_page >}}

# Warum Tests schreiben?

Tests sind ein wichtiger Bestandteil des Entwicklungsprozesses in der Java-Programmierung. Sie dienen dazu, die Qualität und Stabilität des Codes zu gewährleisten. Durch das Schreiben von Tests kann man sicherstellen, dass das Programm so funktioniert, wie es soll, und etwaige Fehler frühzeitig erkennen. Es hilft auch dabei, sich an die gewünschte Funktionalität zu halten und Verbesserungspotenziale aufzudecken.

# Wie schreibt man Tests?

Das Schreiben von Tests ist in Java relativ einfach und erfordert nur wenige Schritte. Zunächst muss man ein Test-Framework wie JUnit oder TestNG in das Projekt einbinden. Dann kann man die zu testenden Methoden schreiben und diese mit Assertions versehen, die überprüfen, ob das erwartete Ergebnis zurückgegeben wird. Hier ist ein Beispiel:

```Java
// Testklasse importieren
import org.junit.Test;

// Testklasse deklarieren
public class MeineKlasseTest {

    // Testmethode deklarieren
    @Test
    public void testMethode() {
        // Erwartetes Ergebnis definieren
        int erwartet = 5;
        // Methode aufrufen und Ergebnis speichern
        int ergebnis = MeineKlasse.methode();
        // Überprüfen, ob Ergebnis mit Erwartung übereinstimmt
        assertEquals(erwartet, ergebnis);
    }
}
```

Dieser Codeblock zeigt eine typische Testklasse in JUnit. Es werden ein Testfall definiert, die zu testende Methode aufgerufen und das Ergebnis mit dem erwarteten Wert verglichen. Natürlich gibt es noch viele weitere Möglichkeiten, Tests zu schreiben, aber dies ist ein guter Einstieg.

# Tiefere Einblicke

Das Schreiben von Tests hat viele Vorteile, die über die bloße Fehlererkennung hinausgehen. Mit der Zeit wächst der Testumfang und bildet eine Art Sicherheitsnetz, das verhindert, dass spätere Änderungen im Code unabsichtlich bestehende Funktionalität beeinflussen. Auch die Dokumentationseigenschaft von Tests sollte nicht unterschätzt werden. Sie dienen als lebendige Beispiele der Nutzung von Klassen und Methoden und können somit auch anderen Entwicklern als Orientierungshilfe dienen.

# Siehe auch

- [JUnit](https://junit.org/)
- [TestNG](https://testng.org/)
- [5 Gründe für das Schreiben von Tests](https://www.thoughtworks.com/insights/blog/5-reasons-why-you-should-write-tests)