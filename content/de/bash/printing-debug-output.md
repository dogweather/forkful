---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Bash-Programmierung: Debug-Ausgabe Drucken

## Was & Warum?

Beim Drucken einer Debug-Ausgabe, wird temporäre Information über das Programm und seinen Ablauf ausgegeben. Programmierer nutzen es, um Fehler (Bugs) zu identifizieren und diese zu beheben.

## Wie macht man das:

Hier sind einige einfache Beispiele:

```Bash
# Einfache Ausgabe
echo "Hallo, das ist eine Debug-Ausgabe."

# Ausgabe mit Umgebungsvariablen
echo "Benutzer: $USER"

# Ausgabe mit Zeilentrennung
printf "Zeile 1\nZeile 2\n"
```

## Vertiefung:

Historisch gesehen stammt die Praxis des Debug-Drucks aus den frühen Tagen der Programmierung, als dedizierte Debugging-Tools weniger weit verbreitet waren. Alternativen umfassen heute leistungsfähige Debugger wie GDB für C/C++ oder PDB für Python. In Bash sind `echo` und `printf` die effektivsten Wege, um Debug-Ausgaben zu drucken, wobei `printf` mehr Formatierungsoptionen bietet.

## Siehe auch:

- GNU Bash-Dokumentation: https://www.gnu.org/software/bash/
- Debugging mit Bash: https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-12.html
- Alternativen zu `echo` und `printf`: https://linuxhint.com/printf_vs_echo_in_bash/