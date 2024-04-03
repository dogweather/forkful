---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:47.162652-07:00
description: "Das Abrufen des aktuellen Datums in Bash erfordert die Verwendung von\
  \ integrierten Befehlen, um das Datum und die Uhrzeit in verschiedenen Formaten\u2026"
lastmod: '2024-03-13T22:44:54.069880-06:00'
model: gpt-4-0125-preview
summary: Das Abrufen des aktuellen Datums in Bash erfordert die Verwendung von integrierten
  Befehlen, um das Datum und die Uhrzeit in verschiedenen Formaten anzuzeigen.
title: Den aktuellen Datum abrufen
weight: 29
---

## Wie geht das:
In Bash ist der Befehl `date` Ihr primäres Werkzeug, um das aktuelle Datum und die Uhrzeit zu erhalten. Hier sind einige Beispiele, wie man ihn verwendet:

1. **Das aktuelle Datum und die Uhrzeit im Standardformat abrufen:**

```bash
date
```

*Beispielausgabe:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **Das Ausgabeformat anpassen:** Sie können das Ausgabeformat mit `+%` Format-Spezifikatoren festlegen. Zum Beispiel, um das Datum im YYYY-MM-DD Format anzuzeigen:

```bash
date "+%Y-%m-%d"
```

*Beispielausgabe:*
```
2023-04-05
```

3. **Den aktuellen UNIX-Zeitstempel abrufen:** Der UNIX-Zeitstempel ist die Anzahl der Sekunden seit der Unix-Epoche (1. Januar 1970). Dies ist nützlich für Skripte, die Berechnungen basierend auf Zeitunterschieden durchführen.

```bash
date "+%s"
```

*Beispielausgabe:*
```
1672877344
```

Für diese grundlegende Operation in Bash werden typischerweise keine beliebten Drittanbieter-Bibliotheken verwendet, da der integrierte Befehl `date` umfassende Funktionalitäten bietet. Allerdings könnten Programmierer für fortgeschrittene Datums- und Zeitmanipulationen andere Programmiersprachen oder Werkzeuge verwenden, die Bibliotheken für Datumsarithmetik und -parsen anbieten, wie zum Beispiel das `datetime` Modul von Python.
