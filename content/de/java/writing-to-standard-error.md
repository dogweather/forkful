---
title:                "Java: Schreiben auf den Standardfehler"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf den Standardfehler kann sehr nützlich sein, um Fehler und Warnungen in einem Java-Programm zu erkennen. Es ermöglicht auch die schnelle Diagnose von Problemen während der Entwicklung oder beim Debuggen von Code.

## Wie man es macht

Das Schreiben auf den Standardfehler ist einfach und kann mit der Methode `System.err.println()` erreicht werden. Hier ist ein Beispiel, wie man einen String auf den Standardfehler schreibt:

```Java
System.err.println("Dies ist ein Beispiel");
```

Die Ausgabe wird dann auf der Konsole in roter Farbe angezeigt, um anzuzeigen, dass es sich um einen Fehler oder eine Warnung handelt. Hier ist ein Beispiel für die Ausgabe:

```Java
Dies ist ein Beispiel
```

## Tiefer Eintauchen

Das Schreiben auf den Standardfehler ermöglicht es auch, zusätzliche Informationen wie den Stacktrace eines Fehlers oder die Zeitstempel auszugeben. Hier ist ein Beispiel, wie man dies erreichen kann:

```Java
try {
    // Code ausführen, der einen Fehler verursacht
} catch (Exception e) {
    // Stacktrace auf den Standardfehler schreiben
    e.printStackTrace(System.err);

    // Aktuelles Datum und Zeit auf den Standardfehler schreiben
    System.err.println("Zeitpunkt des Fehlers: " + new Date());
}
```

Die Ausgabe wird dann folgendermaßen aussehen:

```Java
java.lang.NullPointerException
    at MyProgram.method(MyProgram.java:10)
    at MyProgram.main(MyProgram.java:5)
Zeitpunkt des Fehlers: Mon Sep 27 14:30:21 CEST 2021
```

Durch das Schreiben auf den Standardfehler wird die Fehlerbehandlung erleichtert und die Debugging-Zeit verkürzt.

## Siehe auch

- [Oracle Dokumentation zu System.out und System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Artikel von Baeldung über das Schreiben auf den Standardfehler](https://www.baeldung.com/java-standard-error-output)
- [Tutorial von Java Code Geeks zur Verwendung von System.err](https://www.javacodegeeks.com/2015/03/java-io-tutorial-4-3-what-when-and-how-to-use-system-err-for-logging.html)