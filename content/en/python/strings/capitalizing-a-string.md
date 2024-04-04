---
date: 2024-02-03 19:02:34.962078-07:00
description: 'How to: #.'
lastmod: '2024-04-04'
model: gpt-4-0125-preview
summary: '#.'
title: Capitalizing a string
weight: 2
changelog:
- 2024-04-04 - dogweather - edited
---

## How to:


### Using Python's Built-in Method:
Python has a built-in method `.capitalize()` for strings to accomplish this task easily. 

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Output:**
```
Hello world
```

Here's my own customized `capitalize()` I use to build this site. I needed to make sure special words like **HTML** always stay all caps. This also demonstrates [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Capitalize a string, i.e. make the first letter uppercase.
    Handle special cases like "HTML".

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




### Handling Multiple Words:
For scenarios where you want each word in a string to start with a capital letter (such as titles), the `.title()` method can be applied.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Output:**
```
Python Programming Essentials
```

### Using Third-Party Libraries:
While Python’s standard library is equipped for basic string capitalization, libraries like `textblob` can offer more nuanced control, especially for natural language processing.

First, ensure you have `textblob` installed:
```bash
pip install textblob
```

Then, use it to capitalize strings, keeping in mind that `textblob`'s capitalize might work differently based on the context of use:

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

Remember, while the `capitalize()` and `title()` methods are universally useful, leveraging libraries like `textblob` can provide additional flexibility for specific applications.
