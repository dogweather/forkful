---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:31.181748-07:00
description: "Wie geht das: **Lesen einer CSV-Datei Zeile f\xFCr Zeile**."
lastmod: '2024-03-13T22:44:54.082036-06:00'
model: gpt-4-0125-preview
summary: "**Lesen einer CSV-Datei Zeile f\xFCr Zeile**."
title: Arbeiten mit CSV
weight: 37
---

## Wie geht das:
**Lesen einer CSV-Datei Zeile für Zeile**

```bash
while IFS=, read -r spalte1 spalte2 spalte3
do
  echo "Spalte 1: $spalte1, Spalte 2: $spalte2, Spalte 3: $spalte3"
done < beispiel.csv
```

*Beispielausgabe:*

```
Spalte 1: id, Spalte 2: name, Spalte 3: email
...
```

**Filtern von CSV-Zeilen basierend auf einer Bedingung**

Mit `awk` können Sie einfach Zeilen filtern. Um zum Beispiel Zeilen zu finden, in denen die zweite Spalte "Alice" entspricht:

```bash
awk -F, '$2 == "Alice" { print $0 }' beispiel.csv
```

**Ändern eines Spaltenwerts**

Um die zweite Spalte in Großbuchstaben zu ändern:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' beispiel.csv
```

**Sortieren einer CSV-Datei basierend auf einer Spalte**

Sie können eine CSV-Datei basierend auf, sagen wir, der dritten Spalte (numerisch) sortieren:

```bash
sort -t, -k3,3n beispiel.csv
```

**Verwenden von `csvkit` für komplexere Aufgaben**

`csvkit` ist eine Sammlung von Befehlszeilen-Tools zum Konvertieren und Arbeiten mit CSV. Es kann über pip installiert werden.

Zum Konvertieren einer JSON-Datei in CSV:

```bash
in2csv data.json > data.csv
```

Um eine CSV-Datei mit SQL abzufragen:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" beispiel.csv
```

*Hinweis: Die Installation von `csvkit` benötigt Python und kann über `pip install csvkit` erfolgen.*
