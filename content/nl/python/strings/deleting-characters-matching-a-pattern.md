---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "Hoe te: Ik doe dit vaak genoeg dat ik het heb geherstructureerd in deze\
  \ eenvoudige `delete()` functie. Het is ook een goede demonstratie van\u2026"
lastmod: '2024-04-05T21:53:50.385572-06:00'
model: gpt-4-0125-preview
summary: Ik doe dit vaak genoeg dat ik het heb geherstructureerd in deze eenvoudige
  `delete()` functie.
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe te:

```Python
import re

# Voorbeeld string
tekst = "Hallo, Wereld! 1234"

# Verwijder alle cijfers
geen_cijfers = re.sub(r'\d', '', tekst)
print(geen_cijfers)  # Output: "Hallo, Wereld! "

# Verwijder leestekens
geen_leestekens = re.sub(r'[^\w\s]', '', tekst)
print(geen_leestekens)  # Output: "Hallo Wereld 1234"

# Verwijder klinkers
geen_klinkers = re.sub(r'[aeiouAEIOU]', '', tekst)
print(geen_klinkers)  # Output: "Hll, Wrld! 1234"
```

### Mijn aangepaste functie

Ik doe dit vaak genoeg dat ik het heb geherstructureerd in deze eenvoudige `delete()` functie. Het is ook een goede demonstratie van [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(tekst: str, regex: str) -> str:
    """
    >>> delete("Hallo, wereld!", "l")
    'Hao, wereld!'

    >>> delete("Hallo, wereld!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", tekst)
```

## Diepgaande Duik
De praktijk van het verwijderen van tekens die overeenkomen met een patroon in tekst heeft diepe wortels in de informatica, daterend uit de vroege Unix-tools zoals `sed` en `grep`. In Python biedt de `re` module deze mogelijkheid, waarbij reguliere expressies worden gebruikt - een krachtig en veelzijdig hulpmiddel voor tekstverwerking.

Alternatieven voor de `re` module omvatten:
- String methoden zoals `replace()` voor eenvoudige gevallen.
- Externe bibliotheken zoals `regex` voor complexere patronen en betere Unicode-ondersteuning.

Achter de schermen, wanneer je `re.sub()` gebruikt, compileert de Python-interpreter het patroon naar een reeks bytecodes, verwerkt door een staatmachine die direct patroonmatching uitvoert op de invoertekst. Deze bewerking kan hulpbronintensief zijn voor grote strings of complexe patronen, dus prestatieoverwegingen zijn cruciaal voor het verwerken van grote gegevens.

## Zie ook
- [Python `re` module documentatie](https://docs.python.org/3/library/re.html): Officiële documentatie voor reguliere expressies in Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Een uitgebreide gids voor reguliere expressies.
- [Real Python tutorial over regex](https://realpython.com/regex-python/): Toepassingen van reguliere expressies in Python in de praktijk.
