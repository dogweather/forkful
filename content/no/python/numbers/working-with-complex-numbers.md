---
date: 2024-01-26 04:45:31.371734-07:00
description: "Hvordan: Python har innebygd st\xF8tte for komplekse tall. Her er hvordan\
  \ du kan leke med dem."
lastmod: '2024-03-13T22:44:40.354931-06:00'
model: gpt-4-0125-preview
summary: "Python har innebygd st\xF8tte for komplekse tall."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Python har innebygd støtte for komplekse tall. Her er hvordan du kan leke med dem:

```Python
# Å lage komplekse tall
z = 4 + 5j
print(z)  # Utgang: (4+5j)

# Å få tilgang til reelle og imaginære deler
print(z.real)  # Utgang: 4.0
print(z.imag)  # Utgang: 5.0

# Kompleks aritmetikk
w = 1 - 2j
print(z + w)  # Utgang: (5+3j)
print(z - w)  # Utgang: (3+7j)
print(z * w)  # Utgang: (14+2j)
print(z / w)  # Utgang: (-3.6+1.2j)

# Modulus (absolutt verdi)
print(abs(z))  # Utgang: 6.4031242374328485

# Konjugert av et kompleks tall
print(z.conjugate())  # Utgang: (4-5j)
```

## Dypdykk
Komplekse tall ble først konseptualisert av Gerolamo Cardano på 1500-tallet. Python, blant andre programmeringsspråk, behandler komplekse tall som førsteklasses borgere. Dette betyr at de er innebygd i språket, med lette-å-bruke funksjoner, og unngår behovet for å importere eksterne biblioteker for grunnleggende operasjoner.

Likevel, for tunge numeriske beregninger, har Python et bibliotek kalt `cmath`, som er spesifikt for komplekse tall. Det har tilleggsfunksjoner som `exp`, `log`, og trigonometriske operasjoner.

Når Python ikke er nok, kan du vende deg til biblioteker som NumPy, spesielt for matriseoperasjoner som involverer komplekse tall. NumPy gir optimaliserte og vektoriserte operasjoner som er avgjørende for ytelsen i numerisk databehandling.

## Se Også
Sjekk ut disse ressursene for å lære mer:

- Pythons offisielle dokumentasjon om komplekse tall: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Dokumentasjonen for `cmath`-modulen: https://docs.python.org/3/library/cmath.html
- NumPy for håndtering av matriser med komplekse tall: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
