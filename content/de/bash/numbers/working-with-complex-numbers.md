---
date: 2024-01-26 04:36:38.624690-07:00
description: "Komplexe Zahlen bestehen aus einem Realteil und einem Imagin\xE4rteil.\
  \ Programmierer nutzen sie in Bereichen wie Signalverarbeitung, Quantenmechanik\
  \ und\u2026"
lastmod: '2024-03-13T22:44:54.051489-06:00'
model: gpt-4-0125-preview
summary: "Komplexe Zahlen bestehen aus einem Realteil und einem Imagin\xE4rteil."
title: Umgang mit komplexen Zahlen
weight: 14
---

## Wie:
Bash unterstützt komplexe Zahlen nicht nativ. Man verwendet oft ein externes Werkzeug wie `bc` mit seiner `-l`-Option. So berechnen Sie komplexe Zahlen in Bash:

```bash
echo "sqrt(-1)" | bc -l
```

Ausgabe:
```bash
j
```

Multiplikation:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Ausgabe:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Tiefergehend
Komplexe Zahlen gibt es seit dem 16. Jahrhundert, aber Skriptsprachen wie Bash sind ab Werk nicht für mathematische Berechnungen wie komplexe Zahlen optimiert. Daher kommen oft Werkzeuge wie `bc` oder andere Tools wie `awk` zum Einsatz. Einige alternative Sprachen für die Arbeit mit komplexen Zahlen sind Python mit seinem `cmath`-Modul und MATLAB, die beide für fortgeschrittene mathematische Funktionen entwickelt wurden. Was Bash betrifft, so geht es darum, Werkzeuge zu nutzen - `bc` verwendet das kleine 'i', um die imaginäre Einheit darzustellen und unterstützt grundlegende Operationen wie Addition, Subtraktion, Multiplikation und Division.

## Siehe auch
- Das `bc`-Handbuch: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (Alternative zu MATLAB): https://www.gnu.org/software/octave/
- Python `cmath`-Modul: https://docs.python.org/3/library/cmath.html
