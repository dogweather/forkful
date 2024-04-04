---
changelog:
- 2024-04-04 - dogweather - edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "Hur man g\xF6r: #."
lastmod: '2024-04-04T00:26:52.233938-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:


### Använder Pythons inbyggda metod:
Python har en inbyggd metod `.capitalize()` för strängar för att enkelt genomföra denna uppgift.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Utdata:**
```
Hello world
```

Här är min egen anpassade `capitalize()` som jag använder för att bygga denna webbplats. Jag behövde se till att särskilda ord som **HTML** alltid är skrivna med versaler. Detta demonstrerar också [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Gör första bokstaven i en sträng stor, dvs. gör första bokstaven versal.
    Hanterar särskilda fall som "HTML".

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



### Hantera flera ord:
För scenarier där du vill att varje ord i en sträng ska börja med en stor bokstav (som titlar), kan metoden `.title()` användas.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Utdata:**
```
Python Programming Essentials
```

### Använda tredjepartsbibliotek:
Även om Pythons standardbibliotek är utrustat för grundläggande kapitalisering av strängar, kan bibliotek som `textblob` erbjuda en mer nyanserad kontroll, särskilt för bearbetning av naturligt språk.

Se först till att du har `textblob` installerat:
```bash
pip install textblob
```

Använd sedan det för att göra strängar med stora bokstäver, med tanke på att `textblob` kan fungera annorlunda baserat på användningssammanhang:

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

Kom ihåg, medan metoderna `capitalize()` och `title()` är universellt användbara, kan användningen av bibliotek som `textblob` erbjuda ytterligare flexibilitet för specifika tillämpningar.
