---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: 'Hoe te: Python heeft een ingebouwde methode `.capitalize()` voor strings
  om deze taak eenvoudig te voltooien.'
lastmod: '2024-04-04T00:27:00.827878-06:00'
model: gpt-4-0125-preview
summary: ''
title: Een String Met Hoofdletters Maken
weight: 2
---

## Hoe te:

### Met Python's Ingebouwde Methode:
Python heeft een ingebouwde methode `.capitalize()` voor strings om deze taak eenvoudig te voltooien.

```python
my_string = "hallo wereld"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Uitvoer:**
```
Hallo wereld
```

Hier is mijn eigen aangepaste `capitalize()` die ik gebruik om deze site te bouwen. Ik moest ervoor zorgen dat speciale woorden zoals **HTML** altijd volledig in hoofdletters blijven. Dit demonstreert ook [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Maak de eerste letter van een string een hoofdletter.
    Behandel speciale gevallen zoals "HTML".

    >>> capitalize("dit is html, csv, xml, en http (geen REPL).")
    'Dit is HTML, CSV, XML, en HTTP (geen REPL).'

    >>> capitalize("dit is json, VBA, een IDE, en yaml in de CLI.")
    'Dit is JSON, VBA, een IDE, en YAML in de CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```




### Voor Meerdere Woorden:
Voor scenario's waarin je wilt dat elk woord in een string begint met een hoofdletter (zoals titels), kan de methode `.title()` worden toegepast.

```python
my_title = "python programmeren essentials"
title_case = my_title.title()
print(title_case)
```
**Uitvoer:**
```
Python Programmeren Essentials
```

### Gebruikmakend van Externe Bibliotheken:
Hoewel Python's standaardbibliotheek is uitgerust voor basis stringkapitalisatie, kunnen bibliotheken zoals `textblob` meer genuanceerde controle bieden, vooral voor natuurlijke taalverwerking.

Zorg eerst dat je `textblob` hebt geïnstalleerd:
```bash
pip install textblob
```

Gebruik het vervolgens om strings te kapitaliseren, waarbij je in gedachten houdt dat `textblob`'s kapitalisatie mogelijk anders werkt op basis van de gebruikte context:

```python
from textblob import TextBlob

my_sentence = "dit is een test zin"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Uitvoer:**
```
Dit is een test zin
```

Onthoud dat, terwijl de `capitalize()` en `title()` methoden universeel nuttig zijn, het gebruik van bibliotheken zoals `textblob` extra flexibiliteit kan bieden voor specifieke toepassingen.
