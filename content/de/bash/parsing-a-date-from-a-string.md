---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:34:41.177792-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, das Datum in eine nutzbare Form zu bringen, zum Beispiel um es zu vergleichen oder anders zu verwenden. Programmierer machen das, weil Daten oft als Text vorliegen und für automatische Verarbeitung umgewandelt werden müssen.

## So geht's:
Hier sind zwei Beispiele, wie man ein Datum aus einem String in Bash extrahiert und umwandelt:

```Bash
# Beispiel mit date-Befehl
datum_string="2023-03-15 14:00:00"
datum_formatiert=$(date -d "$datum_string" '+%Y-%m-%d')
echo $datum_formatiert
```
Ausgabe:
```
2023-03-15
```

```Bash
# Beispiel mit purem Bash, ohne externe Befehle
datum_string="15.03.2023 14:00"
IFS=' .:' read -r tag monat jahr stunde minute <<< "$datum_string"
echo "$jahr-$monat-$tag"
```
Ausgabe:
```
2023-03-15
```

## Tiefgang
Ursprünglich brauchten wir externe Programme wie `date` oder `awk`, um Datumswerte zu parsen. Bash ab Version 4 hat verbesserte eingebaute Methoden zum String-handling, die einfaches Parsen ermöglichen.

Alternativen:
- `date`: Mächtig, aber nicht in jedem System gleich verfügbar.
- Externe Tools wie `awk` oder `sed`: Funktionsreich, aber überdimensioniert für einfache Aufgaben.
- Pure Bash-Methoden: Begrenzt, aber effizient und portabel.

Wichtig beim Implementieren:
- Achte auf das Datumsformat. `date` unterstützt viele, aber nicht alle!
- Zeitzone und Lokalisierung können das Ergebnis beeinflussen.
- Sicherstellen, dass die Bash-Version die nötigen Features unterstützt.

## Siehe auch
- Bash Manual: https://www.gnu.org/software/bash/manual/
- `date` Manpage: https://man7.org/linux/man-pages/man1/date.1.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
