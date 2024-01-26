---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:35:51.070035-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String ist der Prozess, bei dem Datumswerte aus Textformaten extrahiert werden. Programmierer machen das, um Datumsangaben zu verarbeiten und manipulieren, oft für Anwendungen wie Kalender, Logfiles oder User-Input.

## How to:
```Fish Shell
# Datum aus String parsen
set date_string "2023-04-02"

# Fish verwendet 'string match' und 'date' für einfaches Parsing
set year (string match -r "\d{4}" $date_string)[1]
set month_day (string match -r "\d{2}-\d{2}" $date_string)[1]

# Ausgabe der Ergebnisse
echo "Jahr: $year"
echo "Monat und Tag: $month_day"

# Datum mit 'date' Kommando konvertieren
set full_date (date -j -f "%Y-%m-%d" $date_string +"%A, %d %B %Y")
echo "Vollständiges Datum: $full_date"
```
Sample Output:
```
Jahr: 2023
Monat und Tag: 04-02
Vollständiges Datum: Sunday, 02 April 2023
```

## Deep Dive
Früher mussten Datumswerte oft mühsam per Hand zerlegt werden, bis Werkzeuge wie reguläre Ausdrücke und spezielle Funktionen in Programmiersprachen dies vereinfachten. In Fish Shell bietet das `date` Kommando diverse Möglichkeiten, mit Datums- und Zeitangaben umzugehen, was Fish neben seiner Scripting-Funktionalität nützlich für Dateioperationen und -verwaltung macht. Alternativen in anderen Shells oder Sprachen können komplexer sein, wie zum Beispiel Date-Parsing in Bash, das oft auf externe Tools wie `date` und `awk` angewiesen ist. Fishs Ansatz bleibt einfach: `string match` für das Pattern-Matching und Kombinationen mit `date` für die Formatierung.

## See Also
- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- GNU Coreutils 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- POSIX Shell Command Language: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
