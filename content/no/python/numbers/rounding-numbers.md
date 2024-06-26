---
date: 2024-01-26 03:46:12.942714-07:00
description: 'Hvordan: Her er det grunnleggende om avrunding av tall i Python.'
lastmod: '2024-03-13T22:44:40.356064-06:00'
model: gpt-4-0125-preview
summary: Her er det grunnleggende om avrunding av tall i Python.
title: Avrunding av tall
weight: 13
---

## Hvordan:
Her er det grunnleggende om avrunding av tall i Python:

```python
# Rund av et tall til nærmeste heltall
print(round(8.67))  # Utganger: 9

# Rund av et tall til et spesifisert antall desimaler
print(round(8.67, 1))  # Utganger: 8.7

# Partall avrundes ned og oddetall avrundes opp når de er like nær
print(round(2.5))  # Utganger: 2
print(round(3.5))  # Utganger: 4
```

## Dypdykk
I Python er ikke `round()` bare å kutte av desimaler. Historisk sett følger Python, som mange andre språk, "avrunder halvparten til jevnt" eller "bankmannens avrunding". Dette minimerer kumulativ feil i summer eller gjennomsnitt, noe som er viktig i finansielle beregninger.

For alternativer har du `math.floor()` og `math.ceil()` fra Pythons matte-modul, som drar tall ned eller opp til neste hele tall. Men hvis det er presisjon du er ute etter, lar `decimal`-modulens `quantize()` deg spesifisere avrundingsatferd.

Under panseret håndterer `round()` binære flyttall. Siden noen desimaler ikke kan uttrykkes nøyaktig i binært, kan du få overraskelser med ting som `round(2.675, 2)` som ikke blir `2.68` som forventet. Her kommer `decimal` eller `fractions` inn for høy presisjon.

## Se Også
- Pythons dokumentasjon om innebygde funksjoner: https://docs.python.org/3/library/functions.html#round
- Decimal fastpunkt og flyttallaritmetikk: https://docs.python.org/3/library/decimal.html
- Pythons matte-modul: https://docs.python.org/3/library/math.html
