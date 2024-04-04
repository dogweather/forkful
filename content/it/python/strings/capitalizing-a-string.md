---
changelog:
- 2024-04-04 - dogweather - edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: 'Come fare: #.'
lastmod: '2024-04-04T00:27:01.440817-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Capitalizzare una stringa
weight: 2
---

## Come fare:


### Utilizzando il Metodo Integrato di Python:
Python dispone di un metodo integrato `.capitalize()` per le stringhe per eseguire facilmente questo compito.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Output:**
```
Hello world
```

Ecco il mio `capitalize()` personalizzato che uso per costruire questo sito. Dovevo assicurarmi che parole speciali come **HTML** rimanessero sempre in maiuscolo. Questo dimostra anche i [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Rendi maiuscola la prima lettera di una stringa.
    Gestisci casi speciali come "HTML".

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




### Gestione di Più Parole:
Per scenari in cui si desidera che ogni parola in una stringa inizi con una lettera maiuscola (come i titoli), può essere applicato il metodo `.title()`.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Output:**
```
Python Programming Essentials
```

### Utilizzando Librerie di Terze Parti:
Sebbene la libreria standard di Python sia dotata dei mezzi necessari per la capitalizzazione di base delle stringhe, librerie come `textblob` possono offrire un controllo più sfumato, specialmente per l'elaborazione del linguaggio naturale.

Prima, assicurati di avere `textblob` installato:
```bash
pip install textblob
```

Quindi, utilizzalo per capitalizzare le stringhe, tenendo presente che il comportamento della capitalizzazione di `textblob` potrebbe variare a seconda del contesto di utilizzo:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Output:**
```
This is a test sentence
```

Ricorda, mentre i metodi `capitalize()` e `title()` sono universalmente utili, sfruttare librerie come `textblob` può fornire flessibilità aggiuntiva per applicazioni specifiche.
