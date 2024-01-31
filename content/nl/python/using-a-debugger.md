---
title:                "Een debugger gebruiken"
date:                  2024-01-28T22:09:44.760902-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"

category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
"Een debugger gebruiken" gaat over het stapsgewijs doorlopen van je Python-code om bugs te ontdekken en het gedrag te begrijpen. We doen dit omdat het veel makkelijker is dan gewoon raden waar dingen fout gingen, en het bespaart ons uren van printverklaring purgatorium.

## Hoe:
Laten we het gebruik van `pdb`, Python's ingebouwde debugger, uiteenzetten. Stel je een bestand voor, `buggy.py`, met een stiekeme bug:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Wanneer je dit script uitvoert, verwacht je `8`, maar het geeft gewoon een syntaxfout. Het is tijd voor de debugger!

In je terminal, voer uit:
```bash
python -m pdb buggy.py
```

Je komt in de debugger, en het ziet er zo uit:
```Python
> /pad_naar_bestand/buggy.py(1)<module>()
-> def add_one(number):
```

Gebruik `l(ist)` om meer code te zien, `n(ext)` om naar de volgende regel te gaan, of `c(ontinue)` om het script door te laten lopen. Wanneer je de fout bereikt, zal `pdb` stoppen en je laten inspecteren.

Nadat je `number ++ 1` hebt gecorrigeerd naar `number + 1`, start de debugger opnieuw om de fix te testen.
Onthoud, vrienden laten vrienden niet coderen zonder net. Genoeg gezegd.

## Diepere Duik
Terug in de Donkere Middeleeuwen van programmeren (ook wel bekend als voordat Integrated Development Environments, of IDE’s, overal waren), waren debuggers vaak op zichzelf staande tools die je buiten je teksteditor zou gebruiken. Ze kwamen te hulp door programmeurs in staat te stellen de staat van hun software op verschillende uitvoeringspunten te inspecteren.

Anno 2023 is Python's `pdb` niet de enige optie. Mensen kunnen IDE's zoals PyCharm of Visual Studio Code gebruiken, die hun eigen slimme debuggers ingebouwd hebben. Deze voegen handige functies toe zoals breakpoints die je met een klik kunt instellen, in plaats van cryptische commando's te typen.

Dan is er `ipdb`, een pip-installeerbaar pakket dat de `IPython` goedheid naar het debuggen brengt. Het is als `pdb` op prestatieverbeteraars, met tabvoltooiing en syntaxiskleuring.

Debuggers variëren ook in hun implementatie. Sommige komen heel dicht bij de uitvoering van het programma op het niveau van machine- of byte code. Anderen, zoals veel high-level taal debuggers, draaien de code in een speciale omgeving die variabele statussen monitort en de uitvoeringsstroom controleert.

## Zie Ook
Voor het complete verhaal over Python's eigen debugger, bekijk:
- De `pdb` documentatie: https://docs.python.org/3/library/pdb.html

Als je nieuwsgierig bent naar alternatieven, zullen deze links je goed van dienst zijn:
- `ipdb` repository en gebruiksgids: https://github.com/gotcha/ipdb
- Debuggen met Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- PyCharm debugmogelijkheden: https://www.jetbrains.com/help/pycharm/debugging-code.html

Gelukkige bugjacht!
