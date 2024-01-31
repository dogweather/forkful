---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:12:57.606395-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum und Uhrzeit abzurufen, ist wie nachzusehen, wie spät es ist – nur für Scripts. Nutzbar für Logs, Zeitstempel oder um Zeitspannen zu messen.

## How to:
Das aktuelle Datum und die Uhrzeit in Bash abzurufen ist einfach. Hier ist ein grundlegendes Beispiel:

```Bash
date
```

Sample output:

```
Mi 5. Apr 00:34:07 CEST 2023
```

Mit `date` können wir auch Formate anpassen:

```Bash
date "+%Y-%m-%d %H:%M:%S"
```

Sample output:

```
2023-04-05 00:34:07
```

## Deep Dive
Der `date`-Befehl stammt aus den Anfängen von Unix und ist seitdem ein Standard-Tool in allen Unix-ähnlichen Systemen, einschließlich Linux und macOS. Alternativen sind Skriptsprachen oder Funktionen in Programmiersprachen wie Python's `datetime` oder PHP's `date()` Funktion. Diese sind nützlich, wenn kompliziertere Zeitberechnungen notwendig sind oder das Datum in einer anderen als der Server-Zeitzone benötigt wird. Beim Bash `date` Befehl hängt die Ausgabe von der lokalen Systemuhr und der eingestellten Zeitzone ab.

## See Also
Hier weitere Quellen, um vertiefende Informationen zu bekommen:

- Bash man page: https://www.gnu.org/software/bash/manual/bash.html
- Datum und Zeit im Bash Script: https://www.tldp.org/LDP/abs/html/timedate.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/index.html
