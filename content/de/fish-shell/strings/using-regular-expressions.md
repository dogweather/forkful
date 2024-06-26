---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:40.686838-07:00
description: "Wie: Obwohl die Fish Shell selbst keinen eingebauten Befehl f\xFCr regex\
  \ hat, nutzt sie effektiv externe Befehle wie `grep`, `sed` und `awk`, die regex\u2026"
lastmod: '2024-03-13T22:44:54.297983-06:00'
model: gpt-4-0125-preview
summary: "Obwohl die Fish Shell selbst keinen eingebauten Befehl f\xFCr regex hat,\
  \ nutzt sie effektiv externe Befehle wie `grep`, `sed` und `awk`, die regex unterst\xFC\
  tzen und Ihnen erlauben, regex-Operationen in Ihre Skripte zu integrieren."
title: "Regul\xE4re Ausdr\xFCcke verwenden"
weight: 11
---

## Wie:
Obwohl die Fish Shell selbst keinen eingebauten Befehl für regex hat, nutzt sie effektiv externe Befehle wie `grep`, `sed` und `awk`, die regex unterstützen und Ihnen erlauben, regex-Operationen in Ihre Skripte zu integrieren.

### Grundlegende Musterabgleiche mit `grep`
Suchen Sie nach Zeilen in einer Datei, die einem Muster entsprechen:

```fish
grep '^[0-9]+' myfile.txt
```

Dieser Befehl findet Zeilen, die in `myfile.txt` mit einer oder mehreren Ziffern beginnen.

### Extrahieren & Ersetzen mit `sed`
Extrahieren Sie Telefonnummern aus einer Datei:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Ersetzen Sie alle Vorkommen von "foo" mit "bar" in `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### Verwendung von `string` für einfache Regex
Der `string`-Befehl der Fish Shell unterstützt einfache regex-Operationen wie Abgleich und Ersetzen:

Ein Muster in einer Zeichenkette abgleichen:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Ausgabe:
```
3.1.2
```

Ziffern nach 'fish' mit 'X.X.X' ersetzen:

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Ausgabe:
```
Welcome to fish X.X.X
```

### Fortgeschrittenes Abgleichen mit `awk`
Drucken Sie die zweite Spalte der Daten aus, wo die erste Spalte einem spezifischen Muster entspricht:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Dieser Befehl sucht in `datafile` nach Zeilen, bei denen die erste Spalte mit einem "a" beginnt, gefolgt von einer oder mehreren Ziffern, und druckt die zweite Spalte aus.

Durch die Integration dieser externen Befehle können Programmierer der Fish Shell die volle Leistungsfähigkeit regulärer Ausdrücke für komplexe Textmanipulationsaufgaben nutzen und die nativen Fähigkeiten der Shell erweitern.
