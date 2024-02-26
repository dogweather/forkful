---
date: 2024-01-20 17:44:55.636805-07:00
description: "Das Extrahieren von Teilstrings erm\xF6glicht es dir, bestimmte Textabschnitte\
  \ aus l\xE4ngeren Strings herauszul\xF6sen. Das ist n\xFCtzlich, wenn du Daten bearbeiten\u2026"
lastmod: '2024-02-25T18:49:51.099202-07:00'
model: gpt-4-1106-preview
summary: "Das Extrahieren von Teilstrings erm\xF6glicht es dir, bestimmte Textabschnitte\
  \ aus l\xE4ngeren Strings herauszul\xF6sen. Das ist n\xFCtzlich, wenn du Daten bearbeiten\u2026"
title: Teilstrings extrahieren
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilstrings ermöglicht es dir, bestimmte Textabschnitte aus längeren Strings herauszulösen. Das ist nützlich, wenn du Daten bearbeiten oder spezifische Informationen analysieren möchtest.

## Wie geht das?
Mit Bash kannst du mit Substring-Expansion arbeiten. Schau dir die Beispiele an:

```Bash
# Extrahiere Teilstring ab Position 4, Länge 5
string="HalloWelt"
echo ${string:3:5}
```
Ausgabe: `loWel`

```Bash
# Extrahieren bis zum Ende, ab Position 5
echo ${string:4}
```
Ausgabe: `oWelt`

```Bash
# Verwende Standardwerte, falls Variable nicht gesetzt oder leer ist
default="Standardwert"
echo ${nichtgesetzt:-$default}
```
Ausgabe: `Standardwert`

## Deep Dive
Substring-Extraktion in Bash gab es schon immer, denn das Arbeiten mit Text ist ein Kernbestandteil von Unix-ähnlichen Systemen. Statt Bash könntest du auch `awk`, `sed`, oder `perl` nutzen. Diese Tools sind mächtig, haben eine steilere Lernkurve. In Bash gibt es zwei Hauptmethoden: Substring-Expansion und Parameter Expansion. Für kritische Performance-Anwendungen ist eine andere Programmiersprache als Bash vielleicht die bessere Wahl, da Shell-Skripte langsamer sind als kompilierte Programme.

## Siehe auch
- Die Bash-Handbuchseite zu String-Manipulationen: `man bash`
- Ein ausführliches Tutorial zu `awk`: [https://www.grymoire.com/Unix/Awk.html](https://www.grymoire.com/Unix/Awk.html)
- Ein Überblick über `sed`: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
