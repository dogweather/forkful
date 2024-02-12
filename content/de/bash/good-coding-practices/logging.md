---
title:                "Protokollierung"
aliases:
- /de/bash/logging.md
date:                  2024-01-26T00:59:09.991592-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?

Logging ist die Praxis, Ereignisse, Fehler und andere bedeutsame Informationen von laufenden Prozessen eines Programms in eine Datei oder einen Ausgabestrom aufzuzeichnen. Programmierer tun dies, um das Verhalten ihrer Anwendungen zu verfolgen, Probleme zu debuggen und ein historisches Protokoll der Vorgänge zu führen, das bei zukünftigen Problemlösungen helfen kann.

## Wie geht das:

Im Bash kann Logging so einfach sein wie das Umleiten oder Anhängen der Ausgabe an eine Datei. Hier ein einfaches Beispiel:

```Bash
echo "Das Skript startet..." >> script.log
# Ihre Skriptbefehle hier
echo "Skript beendet am $(date)" >> script.log
```

Für etwas Fortgeschritteneres könnten Sie syslog für systemweites Logging einbinden:

```Bash
logger "Benutzerdefinierte Nachricht von meinem Skript"
```

`logger` sendet eine Log-Nachricht an den syslog-Dienst, der diese dann gemäß der System-syslog-Konfiguration verarbeitet.

Beispiel für eine in `script.log` aufgezeichnete Ausgabe:

```Bash
Das Skript startet...
Skript beendet am Tue Mar 23 09:26:35 PDT 2021
```

## Vertiefung

Historisch gesehen wurde das Logging in Unix-ähnlichen Systemen durch den syslog-Dienst erleichtert, der es verschiedenen Anwendungen und Teilen des Systems ermöglicht, Nachrichten zentral zu protokollieren. Dies ermöglicht die Implementierung eines standardisierten Loggingmechanismus im gesamten System.

Wenn es um Alternativen geht, könnten einige in Betracht ziehen, `syslog-ng` oder `rsyslog` für fortgeschrittenere Logging-Funktionen zu nutzen oder Logs in einer Zeitreihendatenbank für Analysezwecke zu schreiben. Für Anwendungen mit höherem Komplexitätsgrad kann es sogar für eine Skriptsprache wie Bash sinnvoll sein, eine dedizierte Logging-Bibliothek oder Anwendung wie Log4j (im Java-Ökosystem) oder Monolog (in PHP) zu verwenden, die strukturierte und konfigurierbare Logging-Optionen bieten.

Die Art und Weise, wie Sie das Logging implementieren, hängt stark von den Anforderungen Ihrer Anwendung ab. Wenn Sie lediglich eine einfache Ausgabe benötigen, um den Fortschritt des Skripts zu verfolgen, ist das Anhängen von Nachrichten an eine Datei einfach und bequem. Für ein skalierbares und robustes Logging jedoch sollten Sie eine Integration mit einem Logging-System in Erwägung ziehen, das Funktionen wie Log-Rotation, Log-Level und Remote-Logging unterstützt.

## Siehe auch

- Die `man`-Seiten für die Funktionen `logger` und `syslog` sind immer hilfreich, probieren Sie `man logger` oder `man syslog`.
- Für einen vertieften Einblick in das Systemlogging ziehen Sie das Lesen der Dokumentation von `rsyslog` und `syslog-ng` in Betracht.
- Um mehr über den historischen Kontext und die Prinzipien hinter dem Logging in Unix-ähnlichen Systemen zu erfahren, bietet das im RFC 5424 dokumentierte `Syslog`-Protokoll umfassende Informationen.
