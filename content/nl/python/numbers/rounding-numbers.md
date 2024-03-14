---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:53.803267-07:00
description: "Getallen afronden betekent dat ze worden aangepast om dichter bij een\
  \ eenvoudigere of meer significante waarde te liggen. Programmeurs ronden getallen\
  \ af\u2026"
lastmod: '2024-03-13T22:44:50.371372-06:00'
model: gpt-4-0125-preview
summary: "Getallen afronden betekent dat ze worden aangepast om dichter bij een eenvoudigere\
  \ of meer significante waarde te liggen. Programmeurs ronden getallen af\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Getallen afronden betekent dat ze worden aangepast om dichter bij een eenvoudigere of meer significante waarde te liggen. Programmeurs ronden getallen af om resultaten te vereenvoudigen, het aantal decimalen voor weergave te beperken, of voor bepaalde wiskundige doeleinden.

## Hoe te:
Hier is de essentie van het afronden van getallen in Python:

```python
# Een getal afronden op het dichtstbijzijnde gehele getal
print(round(8.67))  # Geeft uit: 9

# Een getal afronden op een gespecificeerd aantal decimalen
print(round(8.67, 1))  # Geeft uit: 8.7

# Even getallen worden naar beneden afgerond en oneven getallen worden naar boven afgerond wanneer ze op gelijke afstand zijn
print(round(2.5))  # Geeft uit: 2
print(round(3.5))  # Geeft uit: 4
```

## Diepere Duik
In Python doet `round()` niet alleen maar decimalen wegknippen. Historisch gezien volgt Python, net als veel andere talen, de "ronde helft naar even" of "bankiers afronding". Dit minimaliseert cumulatieve fouten in sommen of gemiddelden, wat van belang is in financiële berekeningen.

Voor alternatieven, heb je `math.floor()` en `math.ceil()` uit Python's wiskundemodule, die getallen naar beneden of omhoog trekken naar het volgende hele getal. Maar als precisie is wat je zoekt, stelt de `decimal` module's `quantize()` je in staat het afrondingsgedrag te specificeren.

Onder de motorkap gaat `round()` om met binaire zwevende-kommagetallen. Aangezien sommige decimalen niet exact in binair uitgedrukt kunnen worden, kun je verrassingen tegenkomen met zoiets als `round(2.675, 2)` dat niet wordt `2.68` zoals verwacht. Roep in dat geval `decimal` of `fractions` in voor hoge precisie.

## Zie Ook
- Python's documentatie over ingebouwde functies: https://docs.python.org/3/library/functions.html#round
- Decimale vaste punt en zwevende-kommagetalkunde: https://docs.python.org/3/library/decimal.html
- Python's wiskundemodule: https://docs.python.org/3/library/math.html
