---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:07.599494-07:00
description: "Hoe te: Bash ondersteunt van nature geen complexe getallen. Je gebruikt\
  \ vaak een extern hulpmiddel zoals `bc` met zijn `-l` optie. Hier is hoe je complexe\u2026"
lastmod: '2024-03-13T22:44:50.973514-06:00'
model: gpt-4-0125-preview
summary: Bash ondersteunt van nature geen complexe getallen.
title: Werken met complexe getallen
weight: 14
---

## Hoe te:
Bash ondersteunt van nature geen complexe getallen. Je gebruikt vaak een extern hulpmiddel zoals `bc` met zijn `-l` optie. Hier is hoe je complexe getallen in bash verwerkt:

```bash
echo "sqrt(-1)" | bc -l
```

Uitvoer:
```bash
j
```

Vermenigvuldiging:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Uitvoer:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Diepe duik
Complexe getallen bestaan al sinds de 16e eeuw, maar scripttalen zoals Bash zijn niet op voorhand voorzien voor wiskundige berekeningen zoals complexe getallen. Daarom komen `bc` of andere hulpmiddelen zoals `awk` vaak in het spel. Enkele alternatieve talen voor het werken met complexe getallen zijn Python met zijn `cmath` module en MATLAB, die beide zijn gebouwd voor meer geavanceerde wiskundige functies. Wat Bash betreft, het gaat allemaal om het inzetten van hulpmiddelen - `bc` gebruikt de kleine letter 'i' om de imaginaire eenheid te vertegenwoordigen en ondersteunt basisbewerkingen zoals optellen, aftrekken, vermenigvuldigen en delen.

## Zie ook
- De `bc` handleiding: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternatief voor MATLAB): https://www.gnu.org/software/octave/
- Python `cmath` module: https://docs.python.org/3/library/cmath.html
