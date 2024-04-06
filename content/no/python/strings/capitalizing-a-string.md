---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "Hvordan: Python har en innebygd metode `.capitalize()` for strenger\
  \ som gj\xF8r denne oppgaven enkel."
lastmod: '2024-04-05T21:53:41.323164-06:00'
model: gpt-4-0125-preview
summary: "Python har en innebygd metode `.capitalize()` for strenger som gj\xF8r denne\
  \ oppgaven enkel."
title: Stor bokstav i en streng
weight: 2
---

## Hvordan:

### Ved å bruke Pythons innebygde metode:
Python har en innebygd metode `.capitalize()` for strenger som gjør denne oppgaven enkel.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Utdata:**
```
Hello world
```

Her er min egen tilpassede `capitalize()` som jeg bruker for å bygge dette nettstedet. Jeg måtte sørge for at spesielle ord som **HTML** alltid forblir med store bokstaver. Dette demonstrerer også [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Gjør første bokstav i en streng stor. Håndterer spesialtilfeller som "HTML".

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
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

### Håndtering av flere ord:
For scenarier der du ønsker at hvert ord i en streng skal starte med en stor bokstav (som titler), kan `.title()`-metoden brukes.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Utdata:**
```
Python Programming Essentials
```

### Ved å bruke tredjepartsbiblioteker:
Selv om Pythons standardbibliotek er utstyrt for grunnleggende store bokstaver i strenger, kan biblioteker som `textblob` tilby mer nyansert kontroll, spesielt for behandling av naturlig språk.

Først, sørg for at du har `textblob` installert:
```bash
pip install textblob
```

Deretter kan du bruke det til å gjøre bokstaver store, men husk at `textblob` sin store bokstavfunksjon kan fungere annerledes avhengig av brukskontekst:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Utdata:**
```
This is a test sentence
```

Husk, mens metodene `capitalize()` og `title()` er universelt nyttige, kan bruk av biblioteker som `textblob` gi ekstra fleksibilitet for spesifikke applikasjoner.
