---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:12:28.751199-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen breiden het idee van eendimensionale getallenlijnen uit naar een tweedimensionaal complex vlak. Programmeurs gebruiken ze in velden zoals engineering, natuurkunde en grafische vormgeving voor berekeningen die twee componenten vereisen, zoals signalen of rotaties.

## Hoe te:
In Fish behandelen we complexe getallen met behulp van `math` met reële en imaginaire delen. Hier is een startpunt:

```fish
# Twee complexe getallen optellen (3+4i) en (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Geeft uit: 8+6i

# Twee complexe getallen vermenigvuldigen (1+2i) en (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Geeft uit: -5+10i
```

Als je een complex getal tot een macht moet verheffen of de exponentiële vorm nodig hebt:

```fish
# Kwadraat van (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Geeft uit: -5+12i

# Exponentieel van (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Geeft uit: -0.41615+0.9093i
```

## Diepere Duik
De ondersteuning van Fish Shell voor complexe getallen is relatief nieuw, begonnen rond versie 3.1.0. Daarvoor zouden mensen `bc` kunnen hebben gebruikt of externe tools zoals Python hebben aangesproken voor complexe wiskunde.

Alternatieven voor de `math` van Fish zijn gespecialiseerde numerieke bibliotheken of talen zoals MATLAB, Python met NumPy, of zelfs C++ met de Standard Library. Echter, deze kunnen overkill zijn voor snelle shellberekeningen.

De ondersteuning van complexe getallen in Fish is ingebakken in zijn interne `math`-opdracht, waarbij gebruik wordt gemaakt van libcalc. Dit betekent dat je geen extra tools hoeft te installeren voor basisbewerkingen.

Echter, Fish is niet ontworpen voor zware wiskundige berekeningen. Zijn wiskundige mogelijkheden zijn handig voor snelle berekeningen of scripts waar complexe getallen aan de orde komen, maar overweeg robuustere tools voor intensieve taken.

## Zie Ook
- Fish shell documentatie voor math: https://fishshell.com/docs/current/commands.html#math
- NumPy voor Python, een populaire alternatief: https://numpy.org/
- Een diepere kijk in complexe getallen: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
