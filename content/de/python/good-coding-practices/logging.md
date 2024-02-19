---
aliases:
- /de/python/logging/
date: 2024-01-26 01:08:10.869886-07:00
description: "Logging ist der Prozess der Aufzeichnung von Anwendungsereignissen,\
  \ w\xE4hrend ein Programm l\xE4uft, und bietet eine Art Spur f\xFCr die Analyse\
  \ nach dem Versagen\u2026"
lastmod: 2024-02-18 23:09:04.468826
model: gpt-4-1106-preview
summary: "Logging ist der Prozess der Aufzeichnung von Anwendungsereignissen, w\xE4\
  hrend ein Programm l\xE4uft, und bietet eine Art Spur f\xFCr die Analyse nach dem\
  \ Versagen\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?
Logging ist der Prozess der Aufzeichnung von Anwendungsereignissen, während ein Programm läuft, und bietet eine Art Spur für die Analyse nach dem Versagen und für die Echtzeitüberwachung. Programmierer nutzen es, weil es hilft, Probleme zu debuggen, die Leistung zu überwachen und Nutzeraktionen aus Gründen der Sicherheit und Analyse nachzuverfolgen.

## Wie:
Python kommt mit einem integrierten Modul für das Logging. Hier ist eine Basis-Konfiguration:
```Python
import logging

# Grundkonfiguration des Loggings
logging.basicConfig(level=logging.INFO)

# Log-Nachrichten
logging.debug('Dies ist eine Debug-Nachricht')
logging.info('Informationen darüber, was Ihr Programm gerade getan hat')
logging.warning('Eine Warnung')
logging.error('Ein Fehler ist aufgetreten')
logging.critical('Das Programm kann sich nicht erholen!')
```
Wenn Sie diesen Code ausführen, sehen Sie folgende Ausgabe (da die Standardstufe WARNUNG ist, werden Debug- und Info-Nachrichten nicht angezeigt):
```
WARNING:root:Eine Warnung
ERROR:root:Ein Fehler ist aufgetreten
CRITICAL:root:Das Programm kann sich nicht erholen!
```
Sie können das Logging auch so einstellen, dass in eine Datei statt in die Konsole geschrieben wird:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Nun werden Ihre Logs in die Datei 'app.log' umgeleitet.

## Vertiefung
Logging gibt es seit den Anfängen des Programmierens, wobei Systemprotokolle eine der ältesten Formen von persistentem Speicher außerhalb von tatsächlichen Daten enthaltenden Dateien sind. Abgesehen von der Geschichte bleibt das Hauptkonzept des Loggings im Wesentlichen unverändert, obwohl sich die Werkzeuge weiterentwickelt haben.

Das `logging`-Modul von Python ist ziemlich leistungsfähig und flexibel. Es ermöglicht Programmierern, verschiedene Protokollebenen (DEBUG, INFO, WARNING, ERROR, CRITICAL) festzulegen, die beim Kategorisieren und Filtern von Logs helfen können. Es hat ein hierarchisches Logger-System, was bedeutet, dass Sie Eltern-Kind-Beziehungen zwischen Loggern haben können und Nachrichten die Kette hoch propagieren können.

Alternativen umfassen Drittanbieter-Bibliotheken wie Loguru oder structlog, welche erweiterte Funktionen und eine einfachere Schnittstelle als das integrierte Logging-Modul bieten. Sie können eine hübschere Ausgabe, eine bessere Serialisierung von strukturierten Daten und intuitivere Wege zur Behandlung der Log-Konfiguration bieten.

In Bezug auf die Implementierung ist es wichtig, das Logging gleich zu Beginn Ihrer Anwendung einmal einzurichten. Es wird empfohlen, dies auf Modulebene mit `logging.getLogger(__name__)` zu tun, um den Best Practices des Python-Loggings zu folgen.

Logging sollte unter normalen Umständen die Leistung einer Anwendung nicht drastisch beeinflussen. Dennoch sollte darauf geachtet werden, was protokolliert wird: Zu ausführliches Logging, insbesondere auf DEBUG-Ebene, kann eine Anwendung verlangsamen und schnell den Speicherplatz für Log-Dateien füllen.

## Siehe auch
Für mehr Informationen über das Logging-Modul von Python, schauen Sie sich das offizielle Python Logging-Kochbuch an, das einige tolle Beispiele und Best Practices bietet: https://docs.python.org/3/howto/logging-cookbook.html

Für einen tieferen Einblick in strukturiertes Logging und wie es helfen kann, Logs informativer und einfacher zu analysieren zu machen, ist Loguru gut dokumentiert: https://loguru.readthedocs.io

Betrachten Sie auch die 12-Faktor-App-Methodologie, speziell den Abschnitt über Logs für die moderne Sichtweise auf App-Logging: https://12factor.net/logs
