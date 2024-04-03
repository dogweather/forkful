---
date: 2024-01-26 01:08:35.511846-07:00
description: "Wie geht das: In Swift k\xF6nnen Sie Logs mit `print`-Anweisungen oder\
  \ der flexibleren `os.log`-API in die Konsole schreiben, welche sich in das Unified\u2026"
lastmod: '2024-03-13T22:44:54.231742-06:00'
model: gpt-4-1106-preview
summary: "In Swift k\xF6nnen Sie Logs mit `print`-Anweisungen oder der flexibleren\
  \ `os.log`-API in die Konsole schreiben, welche sich in das Unified Logging System\
  \ auf Apple-Plattformen einklinkt."
title: Protokollierung
weight: 17
---

## Wie geht das:
In Swift können Sie Logs mit `print`-Anweisungen oder der flexibleren `os.log`-API in die Konsole schreiben, welche sich in das Unified Logging System auf Apple-Plattformen einklinkt.

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // Einfache Print-Anweisung
    print("Fetch gestartet")
    
    // Loggen eines Info-Level-Ereignisses mit os.log
    os_log(.info, log: logger, "Daten werden von API abgerufen.")
    
    do {
        let data = try performNetworkRequest()
        // Loggen eines Debug-Level-Ereignisses
        os_log(.debug, log: logger, "Daten empfangen: %@", data.description)
    } catch {
        // Loggen eines Error-Level-Ereignisses
        os_log(.error, log: logger, "Datenabruf fehlgeschlagen: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simulation eines Netzwerkaufrufs
    return Data()
}
```

Eine beispielhafte Ausgabe in der Konsole könnte so aussehen:

```
Fetch gestartet
Daten werden von API abgerufen.
Daten empfangen: Einige Datenbytes...
```

Bei Fehlern könnte sie so aussehen:

```
Datenabruf fehlgeschlagen: Die Internetverbindung scheint offline zu sein.
```

## Vertiefung
Das Loggen in Swift gewinnt mit dem Unified Logging System, das in iOS 10 und macOS Sierra eingeführt wurde, an neuer Kraft und Effizienz. Anders als die `print`-Anweisung, die direkt zur Konsole geht, ist dieses System aktivitätsbasiert und erlaubt Ihnen, Log-Nachrichten auf der Grundlage ihrer Wichtigkeit und ob sie Debug- oder Release-Builds sind, zu filtern.

Der historische Kontext rahmt die Evolution des Loggens in iOS und macOS von rudimentären Print-Anweisungen hin zu umfassenden Werkzeugen, die sich in die Apps "Instruments" und "Konsole" integrieren, und bieten ausgefeilte Wege zur Analyse der Logs.

Es gibt eine Reihe von Alternativen zum Loggen innerhalb von Swift, wie zum Beispiel Drittanbieter-Bibliotheken wie CocoaLumberjack, die eine Makroschicht über dem Unified Logging System bietet. Es ermöglicht eine verbesserte Kontrolle über Log-Formatierung, Dateiverwaltung und Leistungsoptionen.

Zuletzt zu den Implementierungsdetails; OSLog ist so konzipiert, dass es nicht nur effizient, sondern auch datenschutzbewusst ist, mit der Fähigkeit, private Daten beim Loggen zu verschleiern. Es kategorisiert Logs in Fehler-, Fehler-, Info- und Debug-Level, die jeweils eine unterschiedliche Detailgenauigkeit für die Fehlersuche bieten.

## Siehe auch
- [Apples Unified Logging Dokumentation](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlich Logging Tutorial](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjack GitHub-Repository](https://github.com/CocoaLumberjack/CocoaLumberjack)
