---
date: 2024-01-26 04:37:03.267326-07:00
description: "Hvordan: Bash st\xF8tter ikke komplekse tall naturlig. Du vil ofte bruke\
  \ et eksternt verkt\xF8y som `bc` med dens `-l`-alternativ. Slik knuser du komplekse\
  \ tall\u2026"
lastmod: '2024-03-13T22:44:40.966274-06:00'
model: gpt-4-0125-preview
summary: "Bash st\xF8tter ikke komplekse tall naturlig."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Bash støtter ikke komplekse tall naturlig. Du vil ofte bruke et eksternt verktøy som `bc` med dens `-l`-alternativ. Slik knuser du komplekse tall i bash:

```bash
echo "sqrt(-1)" | bc -l
```

Resultat:
```bash
j
```

Multiplikasjon:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Resultat:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Dypdykk
Komplekse tall har eksistert siden det 16. århundret, men skriptspråk som Bash er ikke primært for matematiske beregninger som komplekse tall rett ut av boksen. Det er derfor `bc` eller andre verktøy som `awk` ofte kommer i spill. Noen alternativ språk for arbeid med komplekse tall er Python med dens `cmath`-modul og MATLAB, som begge er bygget for mer avanserte matematiske funksjoner. Når det gjelder Bash, handler det alt om å utnytte verktøy - `bc` bruker den lille 'i'en for å representere den imaginære enheten og støtter grunnleggende operasjoner som addisjon, subtraksjon, multiplikasjon og divisjon.

## Se Også
- `bc`-håndboken: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternativ for MATLAB): https://www.gnu.org/software/octave/
- Python `cmath`-modulen: https://docs.python.org/3/library/cmath.html
