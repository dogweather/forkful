---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Als Programmierer kann es erforderlich sein, das aktuelle Datum zu ermitteln - sei es zur zeitbezogenen Protokollierung, zur Generierung einzigartiger IDs oder für jegliche Datums-/Zeitstempelverwendung. Bash ermöglicht es uns, das aktuelle Datum einfach und effizient zu erfassen.

## So geht's:

Bash-Benutzer können das date-Kommando verwenden, um das aktuelle Datum zu bekommen. Hier ist ein Beispiel:

```Bash
datum=$(date)
echo "Aktuelles Datum: $datum"
```
Wenn man den obigen Code ausführt, erhält man eine Ausgabe ähnlich dieser:

```Bash
Aktuelles Datum: Di Feb 8 12:01:53 CET 2022
```
##Tieftauchgang

Historisch gesehen ist `date` ein altes Unix-Programm, das schon seit den Anfängen dabei ist und in der Bourne-Shell eingepackt ist. Es gibt viele Möglichkeiten, die Nutzung des date-Befehls anzupassen. Mit der Verwendung von Formatierungsoptionen können Benutzer das Datum und die Uhrzeit in nahezu jedem gewünschten Format anzeigen.

Alternativ zu `date` können Sie auch die Funktion `strftime` aus dem `time.h`-Header in C oder `DateTime.Now` in C# verwenden. Bash bietet jedoch eine einfachere Möglichkeit, diese Aufgabe direkt aus der Kommandozeile zu erfüllen.

## Auch Sehen

Weitere Informationen zur Verwendung des date-Befehls und zur Formatierung von Datum und Uhrzeit finden Sie in den Unix/Linux-Handbüchern:
* [Date Command in Linux with examples](https://www.geeksforgeeks.org/date-command-linux-examples/)
* [How To Format Date For Display or Use In a Shell Script](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)