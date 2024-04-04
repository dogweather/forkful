---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Hoe: .'
lastmod: '2024-04-04T01:27:46.308543-06:00'
model: gpt-4-0125-preview
summary: .
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe:

```Python
import re

# Voorbeeld string
tekst = "Hallo, Wereld! 1234"

# Verwijder alle cijfers
geen_cijfers = re.sub(r'\d', '', tekst)
print(geen_cijfers)  # Uitvoer: "Hallo, Wereld! "

# Verwijder leestekens
geen_leestekens = re.sub(r'[^\w\s]', '', tekst)
print(geen_leestekens)  # Uitvoer: "Hallo Wereld 1234"

# Verwijder klinkers
geen_klinkers = re.sub(r'[aeiouAEIOU]', '', tekst)
print(geen_klinkers)  # Uitvoer: "Hll, Wrld! 1234"
```

### Een door mij geschreven aangepaste functie

Ik doe dit vaak genoeg dat ik het heb omgevormd tot deze `delete()` functie. Het is ook een goede demonstratie van [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(tekst: str, regex: str) -> str:
    """
    >>> delete("Hallo, wereld!", "l")
    'Heo, wered!'

    >>> delete("Hallo, wereld!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", tekst)
```

## Diepgaande duik
De praktijk van het verwijderen van tekens die overeenkomen met een patroon in tekst heeft diepe wortels in de informatica, die teruggaan naar vroege Unix-tools zoals `sed` en `grep`. In Python biedt de `re` module deze mogelijkheid, waarbij gebruik wordt gemaakt van reguliere expressies - een krachtige en veelzijdige tool voor tekstverwerking.

Alternatieven voor de `re` module zijn onder andere:
- Stringmethoden zoals `replace()` voor eenvoudige gevallen.
- Externe bibliotheken zoals `regex` voor complexere patronen en betere Unicode-ondersteuning.

Achter de schermen, wanneer je `re.sub()` gebruikt, compileert de Python-interpreter het patroon in een reeks bytecode, die wordt verwerkt door een toestandsmachine die direct patroonmatching uitvoert op de invoertekst. Deze bewerking kan hulpbronnintensief zijn voor grote strings of complexe patronen, dus prestatieoverwegingen zijn cruciaal voor de verwerking van grote gegevens.

## Zie ook
- [Python `re` module documentatie](https://docs.python.org/3/library/re.html): Officiële documentatie voor reguliere expressies in Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Een uitgebreide gids voor reguliere expressies.
- [Real Python tutorial over regex](https://realpython.com/regex-python/): Praktische toepassingen van reguliere expressies in Python.
