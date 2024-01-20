---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Debug-Ausgaben helfen, den Code besser zu verstehen, indem sie wichtige Informationen während der Ausführung anzeigen. Programmierer nutzen dies, um Fehler zu finden und die Funktionsweise ihres Codes zu überprüfen.

## So geht's:

Java bietet verschiedene Methoden zum Drucken von Debug-Ausgaben. Eine der einfachsten Möglichkeiten ist die Verwendung der Methode `System.out.println()`. Hier ist ein einfacher Code-Ausschnitt:

```Java
public class DebugBeispiel {
    public static void main(String[] args) {
        int summe = 0;
        for (int i = 1; i <= 5; i++) {
            summe += i;
            System.out.println("Iteration: " + i + ", Summe: " + summe);
        }
    }
}
```
Wenn Sie diesen Code ausführen, wird die folgende Ausgabe angezeigt:

```Java
Iteration: 1, Summe: 1
Iteration: 2, Summe: 3
Iteration: 3, Summe: 6
Iteration: 4, Summe: 10
Iteration: 5, Summe: 15
```

## Tiefere Eintauchen:

Historisch gesehen stammt die Methode `System.out.println()` aus dem C-Äquivalent `printf()`. Seitdem hat Java weitere Tools für besseres Debugging wie `java.util.logging` und `Log4j` hinzugefügt.

Es gibt verschiedene Alternativen zum Drucken von Debug-Ausgaben, etwa das Schreiben in eine Datei oder das Senden der Ausgabe an eine GUI. Die Wahl hängt vom Anwendungsfeld und den Anforderungen ab.

Bei der Implementierung von Debug-Ausgaben ist es wichtig, sorgfältige Überlegungen anzustellen, um sensible Daten zu schützen und die Leistung nicht zu beeinträchtigen. Es wird empfohlen, ein Logging-Framework zu verwenden, um Ausgaben mit verschiedenen Prioritäten zu verwalten.

## Siehe auch:

- Oracle Java-Dokumentation: `System.out.println()` : [Link](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html#println--)
- Java Logging Basics : [Link](https://www.loggly.com/ultimate-guide/java-logging-basics/)
- Log4j 2 API: [Link](https://logging.apache.org/log4j/2.x/manual/api.html)