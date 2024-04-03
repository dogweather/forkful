---
date: 2024-01-26 03:43:56.513453-07:00
description: "Das Runden von Zahlen besteht darin, Dezimalstellen abzuschneiden, um\
  \ Ihre Daten zu vereinfachen oder spezifischen Formaten anzupassen. Programmierer\
  \ tun\u2026"
lastmod: '2024-03-13T22:44:54.302579-06:00'
model: gpt-4-0125-preview
summary: Das Runden von Zahlen besteht darin, Dezimalstellen abzuschneiden, um Ihre
  Daten zu vereinfachen oder spezifischen Formaten anzupassen.
title: Zahlen runden
weight: 13
---

## Wie:
In Fish hängt das Runden von Zahlen vom Befehl `math` ab. Verwenden Sie `math -s0`, um auf die nächste ganze Zahl zu runden.

```fish
# Aufrunden
echo (math -s0 "4.7")
# Ausgabe: 5

# Abrunden
echo (math -s0 "4.3")
# Ausgabe: 4

# Auf zwei Dezimalstellen runden
echo (math -s2 "4.5678")
# Ausgabe: 4.57

# Negative Zahl runden
echo (math -s0 "-2.5")
# Ausgabe: -3
```

## Tiefer Eintauchen
Historisch wurde das Runden von Zahlen manueller durchgeführt oder mit externen Werkzeugen, aber in modernen Shells wie Fish ist es in die eingebauten Dienstprogramme integriert. Fishs Ansatz unter Verwendung des `math` Befehls vereinfacht die Dinge im Vergleich zu älteren Shells. Alternativen in anderen Programmierumgebungen variieren; Sprachen wie Python verwenden Funktionen wie `round()`, während Bash möglicherweise komplexere Ausdrücke oder das Utility `bc` benötigt. Fishs Implementierung des Rundens vereinfacht das Scripting, indem die Mathematik innerhalb der Shell-Umgebung gehalten wird, anstatt andere Werkzeuge oder Sprachen aufzurufen.

## Siehe auch
- Fish Dokumentation für den Befehl `math`: https://fishshell.com/docs/current/cmds/math.html
- IEEE-Standard für Gleitkomma-Arithmetik (IEEE 754): https://ieeexplore.ieee.org/document/4610935
