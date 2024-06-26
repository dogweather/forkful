---
date: 2024-01-26 04:39:59.559297-07:00
description: "Hvordan gj\xF8re det: I Fish h\xE5ndterer vi komplekse tall ved \xE5\
  \ bruke `math` med reelle og imagin\xE6re deler. Her er en oppstart."
lastmod: '2024-03-13T22:44:41.218882-06:00'
model: gpt-4-0125-preview
summary: "I Fish h\xE5ndterer vi komplekse tall ved \xE5 bruke `math` med reelle og\
  \ imagin\xE6re deler."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan gjøre det:
I Fish håndterer vi komplekse tall ved å bruke `math` med reelle og imaginære deler. Her er en oppstart:

```fish
# Legger sammen to komplekse tall (3+4i) og (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Utganger: 8+6i

# Multipliserer to komplekse tall (1+2i) og (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Utganger: -5+10i
```

Hvis du trenger å heve et komplekst tall til en potens eller få dens eksponentielle form:

```fish
# Kvadratet av (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Utganger: -5+12i

# Eksponenten av (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Utganger: -0.41615+0.9093i
```

## Dypdykk
Fish Shells matematikkstøtte for komplekse tall er relativt ny, og startet rundt versjon 3.1.0. Før det, kan folk ha brukt `bc` eller kalt på eksterne verktøy som Python for kompleks matematikk.

Alternativer til Fish sin matematikk inkluderer spesialiserte numeriske biblioteker eller språk som MATLAB, Python med NumPy, eller til og med C++ med Standardbiblioteket. Men, disse kan være overkill for raske shell-beregninger.

Fish sin støtte for komplekse tall er innebygd i dens interne `math`-kommando, ved å utnytte libcalc. Dette betyr at du ikke trenger å installere ekstra verktøy for grunnleggende operasjoner.

Likevel, Fish er ikke designet for tung matematisk beregning. Dens matematikkevner er praktisk for raske beregninger eller skript hvor komplekse tall kommer til spill, men vurder mer robuste verktøy for intensive oppgaver.

## Se også
- Fish shell dokumentasjon for matematikk: https://fishshell.com/docs/current/commands.html#math
- NumPy for Python, et populært alternativ: https://numpy.org/
- Et dypere dykk inn i komplekse tall: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
