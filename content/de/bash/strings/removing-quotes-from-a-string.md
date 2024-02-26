---
date: 2024-01-26 03:37:39.297029-07:00
description: "Das Entfernen von Anf\xFChrungszeichen aus einem String beinhaltet das\
  \ Entfernen der Anf\xFChrungszeichen, die den String einschlie\xDFen. Programmierer\
  \ m\xF6chten\u2026"
lastmod: '2024-02-25T18:49:51.098257-07:00'
model: gpt-4-0125-preview
summary: "Das Entfernen von Anf\xFChrungszeichen aus einem String beinhaltet das Entfernen\
  \ der Anf\xFChrungszeichen, die den String einschlie\xDFen. Programmierer m\xF6\
  chten\u2026"
title: "Anf\xFChrungszeichen aus einem String entfernen"
---

{{< edit_this_page >}}

## Was & Warum?
Das Entfernen von Anführungszeichen aus einem String beinhaltet das Entfernen der Anführungszeichen, die den String einschließen. Programmierer möchten dies oft tun, um Eingabedaten zu bereinigen, Daten für Vergleichszwecke vorzubereiten oder einem spezifischen Datenformat zu folgen, wenn sie mit anderen Programmen oder Systemen interagieren.

## Wie:
Bash bietet mehrere Möglichkeiten, Anführungszeichen aus Strings zu entfernen. Hier einige schnelle Beispiele:

```Bash
#!/bin/bash

# Verwendung der Variablensubstitution, um sowohl einfache als auch doppelte Anführungszeichen zu entfernen
STRING="\"Hallo, Welt!\""
echo ${STRING//\"}

# Verwendung von `tr`, um Anführungszeichen zu löschen
STRING="'Hallo, Welt!'"
echo $STRING | tr -d "\'"

# Verwendung von `sed`, um Anführungszeichen zu löschen
STRING="\"Hallo, Welt!\""
echo $STRING | sed 's/"//g'
```

Beispielausgabe:

```
Hallo, Welt!
Hallo, Welt!
Hallo, Welt!
```

## Tiefergehend
Schon vor langer Zeit waren Unix-Befehle wie `tr` und `sed` die Hauptwerkzeuge für die Textverarbeitung. Sie werden auch heute noch aufgrund ihrer Flexibilität und Leistungsfähigkeit bei der Handhabung von Texttransformationen wie dem Entfernen von Anführungszeichen verwendet. Sie sind ein fester Bestandteil in jeder Werkzeugkiste eines Shell-Skripters.

Bash selbst hat sich seitdem weiterentwickelt und die Variablensubstitution fügt eine weitere Schicht der Einfachheit für kleinere Stringmanipulationen hinzu. Sie erspart Ihnen das Herauspumpen zu externen Binärdateien und macht Ihre Skripte ein bisschen effizienter.

Während `tr` großartig für das Löschen von Zeichen ist, kann es komplexere Muster nicht bearbeiten. `sed` hingegen verwendet reguläre Ausdrücke, daher ist es manchmal übertrieben und könnte für einfache Operationen langsamer sein.

Die Wahl zwischen diesen Methoden hängt von Ihrem spezifischen Fall ab. Wenn Sie eine Vielzahl von Anführungszeichen entfernen müssen und Sie sich bereits im Kontext eines Bash-Skripts befinden, ist die Verwendung der Variablensubstitution aufgrund ihrer Einfachheit ein Kinderspiel. Aber wenn Sie Textströme oder mehrzeilige Daten umwandeln, sind `tr` und `sed` Ihre besten Freunde.

## Siehe auch:
- Das GNU Bash-Handbuch, insbesondere die Abschnitte zu Parametererweiterung und Shell-Parametererweiterung: https://www.gnu.org/software/bash/manual/
- Das Handbuch zum Befehl `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Die Übersicht zum `sed` Stream-Editor: https://www.gnu.org/software/sed/manual/sed.html
