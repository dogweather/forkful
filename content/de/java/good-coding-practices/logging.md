---
date: 2024-01-26 01:06:24.265245-07:00
description: "Logging ist im Grunde der Prozess der Aufzeichnung von Ereignissen,\
  \ die innerhalb einer Softwareanwendung auftreten. Programmierer protokollieren\
  \ diese\u2026"
lastmod: '2024-03-13T22:44:53.769108-06:00'
model: gpt-4-1106-preview
summary: Logging ist im Grunde der Prozess der Aufzeichnung von Ereignissen, die innerhalb
  einer Softwareanwendung auftreten.
title: Protokollierung
weight: 17
---

## Was & Warum?
Logging ist im Grunde der Prozess der Aufzeichnung von Ereignissen, die innerhalb einer Softwareanwendung auftreten. Programmierer protokollieren diese Ereignisse, um Laufzeitinformationen zu erfassen, Probleme zu debuggen, das Systemverhalten zu überwachen und eine Audit-Trail für Sicherheits- und Compliance-Zwecke zu erstellen.

## Wie geht das:
Hier ist eine einfache Art, mit dem Logging in Java unter Verwendung des eingebauten `java.util.logging`-Pakets zu beginnen.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Protokollierung einer INFO-Level-Nachricht");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Ausnahme aufgetreten", e);
        }
    }
}
```

Das würde eine Ausgabe in etwa wie folgt erzeugen:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Protokollierung einer INFO-Level-Nachricht
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Ausnahme aufgetreten
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## Vertiefung
Logging in Java hat sich ziemlich weiterentwickelt. Historisch gesehen war Logging eher ad-hoc mit Systemausgaben und selbstgeschriebenen Mechanismen. Der Bedarf an Standardisierung führte jedoch zu Logging-APIs wie `Log4j` und `SLF4J`. Das `java.util.logging`-Paket wurde mit JDK 1.4 eingeführt und bietet eine standardisierte Möglichkeit, Nachrichten zu protokollieren.

Alternativen zu `java.util.logging` (JUL) sind unter anderem Log4j 2 und SLF4J. Während JUL in Java integriert ist und daher keine zusätzlichen Abhängigkeiten erfordert, bieten sowohl Log4j 2 als auch SLF4J fortschrittlichere Funktionen wie granularere Steuerung der Logging-Konfiguration, asynchrones Logging und bessere Leistung.

Implementierungstechnisch kann Logging entweder synchron sein, wobei jede Log-Nachricht im Thread verarbeitet wird, der sie generiert hat, oder asynchron, wobei Nachrichten an einen separaten Thread übergeben werden. Asynchrones Logging kann die Leistung verbessern, führt aber zu Komplexität, da man sich um Nebenläufigkeit kümmern und sicherstellen muss, dass Log-Nachrichten bei einem Absturz der Anwendung nicht verloren gehen.

## Siehe auch
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oracles offizieller Logging-Überblick](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Tutorial zu java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
