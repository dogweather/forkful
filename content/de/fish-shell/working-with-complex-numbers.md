---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:39:58.935922-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen erweitern die Idee von eindimensionalen Zahlengeraden zu einer zweidimensionalen komplexen Ebene. Programmierer nutzen sie in Bereichen wie Ingenieurwesen, Physik und Grafik für Berechnungen, die zwei Komponenten benötigen, wie Signale oder Rotationen.

## Wie zu:
In Fish behandeln wir komplexe Zahlen mit `math` und unterteilen diese in Real- und Imaginärteile. Hier ein Start:

```fish
# Zwei komplexe Zahlen addieren (3+4i) und (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Gibt aus: 8+6i

# Zwei komplexe Zahlen multiplizieren (1+2i) und (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Gibt aus: -5+10i
```

Wenn du eine komplexe Zahl potenzieren oder in ihre Exponentialform bringen möchtest:

```fish
# Quadrat von (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Gibt aus: -5+12i

# Exponentielle von (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Gibt aus: -0.41615+0.9093i
```

## Vertiefung
Die Unterstützung von komplexen Zahlen in Fish Shell ist relativ neu und begann um die Version 3.1.0. Davor haben die Leute vielleicht `bc` verwendet oder externe Tools wie Python für komplexe Mathematik aufgerufen.

Alternativen zu Fishs `math` umfassen spezialisierte numerische Bibliotheken oder Sprachen wie MATLAB, Python mit NumPy oder sogar C++ mit der Standardbibliothek. Diese können jedoch für schnelle Berechnungen in der Shell zu umständlich sein.

Die Unterstützung komplexer Zahlen in Fish ist in seinem internen `math`-Befehl integriert, der libcalc nutzt. Das bedeutet, dass man keine zusätzlichen Tools für grundlegende Operationen installieren muss.

Allerdings ist Fish nicht für schwere mathematische Berechnungen konzipiert. Seine mathematische Fähigkeit ist praktisch für schnelle Berechnungen oder Skripte, in denen komplexe Zahlen ins Spiel kommen, aber für intensive Aufgaben sollte man robustere Tools in Betracht ziehen.

## Siehe auch
- Fish-Shell-Dokumentation für math: https://fishshell.com/docs/current/commands.html#math
- NumPy für Python, eine beliebte Alternative: https://numpy.org/
- Ein tieferer Einblick in komplexe Zahlen: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
