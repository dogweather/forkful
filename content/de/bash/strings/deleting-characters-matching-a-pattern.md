---
date: 2024-01-20 17:41:34.253078-07:00
description: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, erm\xF6glicht\
  \ es, Strings in Bash effizient zu bearbeiten. Programmierer nutzen dies, um Eingaben\
  \ zu\u2026"
lastmod: '2024-02-25T18:49:51.094553-07:00'
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, erm\xF6glicht\
  \ es, Strings in Bash effizient zu bearbeiten. Programmierer nutzen dies, um Eingaben\
  \ zu\u2026"
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, ermöglicht es, Strings in Bash effizient zu bearbeiten. Programmierer nutzen dies, um Eingaben zu validieren, Daten zu säubern oder spezifische Formate zu erzeugen.

## Wie geht das?
```Bash
# Variable mit Wert
text="Hallo123Welt456"

# Muster: Zahlen löschen
bereinigter_text=$(echo "$text" | tr -d '0-9')
echo $bereinigter_text
```
Ausgabe:
```
HalloWelt
```

```Bash
# Variable mit Pfad
pfad="/home/user/dokumente/datei.txt"

# Muster: Alles bis zum letzten '/' löschen
ordner_name=${pfad%/*}
echo $ordner_name
```
Ausgabe:
```
/home/user/dokumente
```

## Tiefere Einblicke
Das Löschen bestimmter Zeichen hat in Unix-ähnlichen Systemen eine lange Tradition. Tools wie `tr`, `sed`, `awk` oder Shell-Parametererweiterungen bieten verschiedene Herangehensweisen. Mit `tr` werden Zeichen ersetzt oder entfernt. `sed` (stream editor) ist für komplexere Transformationen gedacht, während `awk` als Programmiersprache für Textverarbeitung gilt.

Für uns heute ist `tr` besonders unkompliziert, wenn es um einfaches Löschen geht. Shell-Parametererweiterungen sind direkt in der Bash integriert und bieten schnelle Änderungen ohne das Aufrufen externer Programme. Beide Methoden haben unterschiedliche Anwendungsgebiete: `tr` ist klar im Vorteil, wenn es um Zeichensätze geht, Shell-Parametererweiterungen, wenn es um Teile von Variableninhalten geht.

## Siehe auch
- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [GNU `tr` manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Bash `sed` introduction](https://www.gnu.org/software/sed/manual/sed.html)
- [Awk User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html)
