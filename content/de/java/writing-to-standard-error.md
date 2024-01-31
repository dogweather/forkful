---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
simple_title:         "Schreiben auf Standardfehler"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standardfehler (stderr) ist ein Ausgabestrom, der Fehler und Diagnosemeldungen anzeigt. Programmierer nutzen stderr, um Fehlermeldungen von normalen Programmausgaben zu trennen, was hilft, Probleme während der Entwicklung und Fehlerbehandlung schneller zu identifizieren.

## How to:
Java verwendet `System.err` für stderr. Hier ist ein einfaches Beispiel, das eine Fehlermeldung ausgibt.

```Java
public class StderrExample {
    public static void main(String[] args) {
        System.err.println("Fehler gefunden: Ungültige Eingabe.");
    }
}
```

Sample Output könnte so aussehen:

```
Fehler gefunden: Ungültige Eingabe.
```

## Deep Dive:
In den Anfängen der Computer gab es oft nur wenige Wege, mit dem Benutzer zu interagieren – meist über die Konsole. Standardfehler ist ein Überbleibsel dieser Zeit, das sich bis heute als nützliches Konzept behauptet hat. Alternativen zu `System.err` könnten die Verwendung von Logging-Frameworks wie Log4j oder das Schreiben in Dateien sein – abhängig von der Anforderung der Anwendung. `System.err` schreibt standardmäßig in die Konsole, kann aber umgeleitet werden, um Fehler in Dateien zu loggen oder anderswohin zu senden.

## See Also:
- [Java Dokumentation für System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Oracle Tutorial zu Ausnahmen](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- [Log4j – Ein populäres Logging-Framework](https://logging.apache.org/log4j/2.x/)
