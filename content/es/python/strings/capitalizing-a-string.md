---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "C\xF3mo hacerlo: Python tiene un m\xE9todo incorporado `.capitalize()`\
  \ para las cadenas que permite realizar esta tarea f\xE1cilmente."
lastmod: '2024-04-05T21:53:59.952148-06:00'
model: gpt-4-0125-preview
summary: "Python tiene un m\xE9todo incorporado `.capitalize()` para las cadenas que\
  \ permite realizar esta tarea f\xE1cilmente."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:


### Usando el Método Incorporado de Python:
Python tiene un método incorporado `.capitalize()` para las cadenas que permite realizar esta tarea fácilmente.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Salida:**
```
Hello world
```

Aquí está mi propio `capitalize()` personalizado que uso para construir este sitio. Necesitaba asegurarme de que palabras especiales como **HTML** siempre se mantengan en mayúsculas. Esto también demuestra [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Poner en mayúscula una cadena, es decir, hacer que la primera letra sea mayúscula.
    Manejar casos especiales como "HTML".

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, un IDE, and YAML in the CLI.'
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




### Manejando Múltiples Palabras:
Para escenarios donde quieras que cada palabra en una cadena comience con una letra mayúscula (como los títulos), se puede aplicar el método `.title()`.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Salida:**
```
Python Programming Essentials
```

### Usando Bibliotecas de Terceros:
Aunque la biblioteca estándar de Python está equipada para la capitalización básica de cadenas, bibliotecas como `textblob` pueden ofrecer un control más matizado, especialmente para el procesamiento de lenguaje natural.

Primero, asegúrate de tener `textblob` instalado:
```bash
pip install textblob
```

Luego, úsalo para poner en mayúsculas las cadenas, teniendo en cuenta que la capitalización de `textblob` podría funcionar de manera diferente basada en el contexto de uso:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Salida:**
```
This is a test sentence
```

Recuerda, mientras que los métodos `capitalize()` y `title()` son útiles universalmente, aprovechar bibliotecas como `textblob` puede proporcionar una flexibilidad adicional para aplicaciones específicas.
