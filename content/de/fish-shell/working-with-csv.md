---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV (Comma-Separated Values) ist ein einfaches Format, um tabellarische Daten zu speichern und auszutauschen. Programmierer nutzen CSV, weil es menschenlesbar und leicht von verschiedenen Programmen und Programmiersprachen zu bearbeiten ist.

## How to:
Beispiel CSV-Datei `daten.csv`:
```
name,alter,stadt
Maria,28,Hamburg
Jens,35,Berlin
Nele,22,Köln
```

CSV-Datei lesen und Inhalt ausgeben:
```Fish Shell
cat daten.csv
```

CSV-Zeilen zählen:
```Fish Shell
cat daten.csv | tail -n +2 | wc -l
```

Vorhandene Stadt in CSV finden und Zeile ausgeben:
```Fish Shell
grep "Köln" daten.csv
```

Ausgabe:
```
Nele,22,Köln
```

CSV nach Alter sortieren:
```Fish Shell
sort --field-separator=',' --key=2 -n daten.csv | tail -n +2
```

Output:
```
Nele,22,Köln
Maria,28,Hamburg
Jens,35,Berlin
```

## Deep Dive
CSV ist ein Format aus den frühen Tagen der Informatik, einfach zu verstehen und weiterhin beliebt wegen seiner Flexibilität und Einfachheit. Alternativen wie XML und JSON bieten mehr Struktur und Möglichkeiten, sind aber komplexer. Fish Shell hat keine eingebauten Funktionen zur speziellen Verarbeitung von CSV, aber Unix-Tools wie `awk`, `sed`, `sort` und `cut` lassen sich leicht integrieren.

## See Also
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Unix-Tools für Textverarbeitung - GNU](https://www.gnu.org/software/coreutils/manual/coreutils.html)
- [CSV-Standard - RFC 4180](https://tools.ietf.org/html/rfc4180)
