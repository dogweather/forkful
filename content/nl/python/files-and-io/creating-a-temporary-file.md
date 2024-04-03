---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:38.671149-07:00
description: 'Hoe: De `tempfile` module van Python is hier speciaal voor ontworpen.
  Kijk hoe het werkt.'
lastmod: '2024-03-13T22:44:50.395688-06:00'
model: gpt-4-0125-preview
summary: De `tempfile` module van Python is hier speciaal voor ontworpen.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Hoe:
De `tempfile` module van Python is hier speciaal voor ontworpen. Kijk hoe het werkt:

```Python
import tempfile

# Creëer een tijdelijk bestand en schrijf er iets naar toe
with tempfile.TemporaryFile(mode='w+t') as tf:
    # Schrijf een string naar het tijdelijke bestand
    tf.write('Python is leuk!')
    # Ga terug naar het begin van het bestand voordat je gaat lezen
    tf.seek(0)
    # Lees wat we hebben geschreven
    print(tf.read())  # Geeft als output: Python is leuk!

# En zo is het bestand weg als je buiten het blok bent
```

Deze code gebruikt een contextmanager om het bestand te beheren, dat automatisch opruimt na zichzelf. Geen achtergebleven bestanden!

## Diepgaand:
Tijdelijke bestanden zijn niet nieuw. Ze worden al sinds de dageraad van de informatica gebruikt om vluchtige gegevens vast te houden. Python's `tempfile` module behandelt de vieze details zoals het genereren van unieke namen en het verwijderen van de bestanden als je klaar bent. Als je nog meer controle wilt, is er `NamedTemporaryFile`, die je kunt refereren bij naam tijdens zijn korte levensduur. Maar onthoud, het doel is om tijdelijk te zijn:

```Python
import tempfile

# Creëer een genoemd tijdelijk bestand
with tempfile.NamedTemporaryFile(delete=True) as ntf:
    print(f'De naam van het tijdelijke bestand is: {ntf.name}')  # Het heeft een daadwerkelijke naam!

# Toch verdwijnt het na gebruik
```

En waarom geen gewone bestanden gebruiken? Simpel: Het gebruiken van `tempfile` bespaart je van rommel en potentiële conflicten — stel je voor dat je script opnieuw draait en dezelfde bestandsnaam wordt hergebruikt. Rommelig, toch?

## Zie Ook:
- Python's tempfile documentatie: https://docs.python.org/3/library/tempfile.html
- Een tutorial over bestandsinvoer/-uitvoer in Python: https://realpython.com/read-write-files-python/
