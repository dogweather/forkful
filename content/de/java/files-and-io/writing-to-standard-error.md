---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:28.224483-07:00
description: "Das Schreiben auf den Standardfehler (stderr) umfasst das Ausgeben von\
  \ Fehlermeldungen und Diagnosen auf die Konsole oder das Terminal. Programmierer\u2026"
lastmod: '2024-03-13T22:44:53.778833-06:00'
model: gpt-4-0125-preview
summary: Das Schreiben auf den Standardfehler (stderr) umfasst das Ausgeben von Fehlermeldungen
  und Diagnosen auf die Konsole oder das Terminal.
title: Schreiben auf Standardfehler
weight: 25
---

## Wie:


### Grundlegende stderr-Ausgabe in Java
Java bietet eine unkomplizierte Möglichkeit, mit `System.err.print()` oder `System.err.println()` auf stderr zu schreiben. So geht's:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Fehler: Kann nicht durch null teilen.");
        }
    }
}
```

Beispielausgabe:

```
Fehler: Kann nicht durch null teilen.
```

Dies wird die Fehlermeldung direkt an den Standardfehlerstrom ausgeben.

### Verwendung eines Loggers für fortgeschrittene Fehlerbehandlung
Für Anwendungen, die eine ausgefeiltere Fehlerbehandlung und Protokollierung benötigen, ist die Verwendung einer Protokollierungsbibliothek wie SLF4J mit Logback oder Log4J2 üblich. Dies ermöglicht mehr Flexibilität bei der Verwaltung der Fehlerausgabe, einschließlich Dateiumleitung, Filterung und Formatierung.

#### Beispiel mit Logback
Fügen Sie zunächst die Abhängigkeit für Logback zu Ihrer `pom.xml` (Maven) oder `build.gradle` (Gradle) Datei hinzu. Für Maven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Dann können Sie den folgenden Code verwenden, um Fehler zu protokollieren:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Fehler: Kann nicht durch null teilen.", e);
        }
    }
}
```

Dies gibt die Fehlermeldung zusammen mit einem Stacktrace auf die Konsole oder in eine Datei aus, abhängig von der Logback-Konfiguration.

Die Verwendung von Protokollierungsframeworks wie Logback bietet mehr Kontrolle über die Fehlerbehandlung und erleichtert das Verwalten von großen Anwendungen und Systemen.
