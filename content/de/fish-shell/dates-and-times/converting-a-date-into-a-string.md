---
title:                "Datum in einen String umwandeln"
aliases:
- /de/fish-shell/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:30.954119-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Umwandeln eines Datums in einen String bedeutet, ein Datum in eine Textform zu bringen, die Menschen lesen können. Programmierer machen das, um Daten benutzerfreundlich anzuzeigen oder zu speichern.

## How to: (Wie geht das:)
Fish hat eingebaute Funktionen, um mit Datum und Zeit zu arbeiten. Hier ein paar Beispiele, wie man ein Datum in Fish in einen String umwandelt.

```Fish Shell
# Aktuelles Datum und Zeit in ISO-8601 Format
set date_string (date -u +"%Y-%m-%dT%H:%M:%SZ")
echo $date_string
```

```Fish Shell
# Benutzerdefiniertes Format, z.B. Tag.Monat.Jahr
set date_string (date +"%d.%m.%Y")
echo $date_string
```

## Deep Dive (Tieftauchgang)
Das Konzept, Daten zu formatieren, stammt aus der Notwendigkeit, Datums- und Zeitinformationen in verschiedenen Kontexten zu präsentieren. Historisch gab es viele Formate, je nach kulturellen oder technischen Anforderungen. In der Unix-Welt ist 'date' seit den Anfängen dabei, um Zeitstrings zu generieren oder auszugeben. 

Alternativen zum 'date'-Befehl wären Programmiersprachen wie Python oder Perl, wo man umfangreiche Datumsmanipulationen vornehmen kann. In Fish erfolgt die Datumsstringgenerierung durch den 'date'-Befehl, der unter der Haube Systemaufrufe nutzt, um die Zeit zu erfahren und zu formatieren.

Fish selbst hat keine eigenen Befehle nur für Datumsoperationen, sondern verlässt sich auf Unix-Kommandos. Die Flexibilität ergibt sich aus der Kombination von 'date' mit Fish's Syntax für Variablen und Strings.

## See Also (Siehe auch)
- Die Fish Shell Dokumentation zum Umgang mit Variablen und String-Manipulation: https://fishshell.com/docs/current/index.html#variables
- GNU 'date' Befehl Dokumentation für tiefergehende Formatoptionen: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Eine Einführung in die Zeitmessung und Darstellung in Unix-Systemen: https://en.wikipedia.org/wiki/Unix_time
