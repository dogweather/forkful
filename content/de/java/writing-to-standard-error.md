---
title:                "Einschreiben auf Standardfehler"
html_title:           "Java: Einschreiben auf Standardfehler"
simple_title:         "Einschreiben auf Standardfehler"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf die Standard Error-Konsole kann hilfreich sein, um Fehler und Warnungen in Ihrem Java-Code zu erkennen und zu beheben. Es ermöglicht auch eine dynamische Kommunikation mit dem Benutzer während der Codeausführung.

## Wie geht das?

Um auf die Standard Error-Konsole zu schreiben, verwenden Sie die Methode `System.err.println()`. Hier ist ein Beispiel in Java:

```
public class Main {
  public static void main(String[] args) {
    System.err.println("Dies ist eine Fehlermeldung.");
  }
}
```

Die Ausgabe auf der Konsole wird folgendermaßen aussehen:

```
Dies ist eine Fehlermeldung.
```

## Tiefere Einblicke

Die Standard Error-Konsole unterscheidet sich von der Standard Output-Konsole in der Art, wie sie Nachrichten behandelt. Während die Standard Output-Konsole für reguläre Ausgaben bestimmt ist, wird die Standard Error-Konsole für Fehlermeldungen und Warnungen verwendet.

Mit `System.err.println()` können Sie auch Ausnahmefehler direkt auf die Konsole schreiben, anstatt sie in eine Log-Datei zu schreiben. Dies kann bei der Fehlerbehebung sehr hilfreich sein, da Sie sofort sehen können, wo und warum der Fehler aufgetreten ist.

Ein weiterer Vorteil der Verwendung der Standard Error-Konsole ist, dass Sie während der Codeausführung Nachrichten an den Benutzer senden können, um ihn über den Fortschritt des Codes oder bestimmte Aktionen zu informieren.

## Siehe auch

- [Java Documentation: System class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html)
- [Difference between System.out.println() and System.err.println() in Java](https://www.tutorialspoint.com/difference-between-system-out-println-and-system-err-println-in-java)
- [Debugging Java Programs using Standard Error Console](https://www.youtube.com/watch?v=S-4mQAMQ0oc)