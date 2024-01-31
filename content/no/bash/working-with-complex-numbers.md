---
title:                "Å jobbe med komplekse tall"
date:                  2024-01-26T04:37:03.267326-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Komplekse tall består av en reell del og en imaginær del. Programmerere bruker dem innen felter som signalbehandling, kvantemekanikk, og når som helst beregningen krever det, fordi vanlige reelle tall bare ikke strekker til.

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
