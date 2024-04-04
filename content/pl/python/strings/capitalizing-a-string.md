---
changelog:
- 2024-04-04 - dogweather - edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "Jak to zrobi\u0107: #."
lastmod: '2024-04-04T00:26:59.072204-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Zamiana liter w ci\u0105gu na wielkie"
weight: 2
---

## Jak to zrobić:

### Korzystając z wbudowanej metody Pythona:
Python posiada wbudowaną metodę `.capitalize()` dla ciągów znaków, która pozwala łatwo wykonać to zadanie.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Wynik:**
```
Hello world
```

Oto mój własny dostosowany `capitalize()`, którego używam do budowania tej strony. Musiałem się upewnić, że specjalne słowa jak **HTML** zawsze są pisane wielkimi literami. To również demonstruje [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Zamienia pierwszą literę ciągu na wielką literę.
    Obsługuje specjalne przypadki jak "HTML".

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

### Obsługa wielu słów:
W przypadkach, gdy chcesz, aby każde słowo w ciągu rozpoczynało się z dużej litery (tak jak w tytułach), można zastosować metodę `.title()`.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Wynik:**
```
Python Programming Essentials
```

### Korzystanie z bibliotek stron trzecich:
Chociaż standardowa biblioteka Pythona jest wyposażona w podstawowe możliwości zmiany wielkości liter ciągów, biblioteki takie jak `textblob` mogą oferować bardziej subtelne sterowanie, szczególnie dla przetwarzania języka naturalnego.

Najpierw upewnij się, że masz zainstalowany `textblob`:
```bash
pip install textblob
```

Następnie, użyj go do zmiany wielkości liter w ciągach, mając na uwadze, że sposób działania capitalize w `textblob` może różnić się w zależności od kontekstu użycia:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Wynik:**
```
This is a test sentence
```

Pamiętaj, choć metody `capitalize()` i `title()` są uniwersalnie użyteczne, korzystanie z bibliotek takich jak `textblob` może zapewnić dodatkową elastyczność dla konkretnych zastosowań.
