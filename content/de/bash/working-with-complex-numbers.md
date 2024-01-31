---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:36:38.624690-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen bestehen aus einem Realteil und einem Imaginärteil. Programmierer nutzen sie in Bereichen wie Signalverarbeitung, Quantenmechanik und immer dann, wenn Berechnungen sie erfordern, weil normale reelle Zahlen einfach nicht ausreichen.

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
