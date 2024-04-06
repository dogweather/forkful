---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "Como Fazer: Python possui um m\xE9todo nativo `.capitalize()` para strings\
  \ que facilita essa tarefa."
lastmod: '2024-04-04T00:26:53.755309-06:00'
model: gpt-4-0125-preview
summary: ''
title: Capitalizando uma string
weight: 2
---

## Como Fazer:


### Usando o Método Nativo do Python:
Python possui um método nativo `.capitalize()` para strings que facilita essa tarefa. 

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Saída:**
```
Hello world
```

Aqui está minha própria versão customizada de `capitalize()` que uso para construir este site. Eu precisei garantir que palavras especiais como **HTML** sempre permanecessem em caixa alta. Isso também demonstra [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Capitaliza uma string, ou seja, faz a primeira letra ficar maiúscula.
    Lida com casos especiais como "HTML".

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




### Lidando com Várias Palavras:
Para cenários onde você quer que cada palavra em uma string comece com uma letra maiúscula (como títulos), o método `.title()` pode ser aplicado.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Saída:**
```
Python Programming Essentials
```

### Usando Bibliotecas de Terceiros:
Embora a biblioteca padrão do Python seja equipada para a capitalização básica de strings, bibliotecas como `textblob` podem oferecer um controle mais matizado, especialmente para o processamento de linguagem natural.

Primeiro, certifique-se de que você tenha o `textblob` instalado:
```bash
pip install textblob
```

Então, use-o para capitalizar strings, tendo em mente que o capitalizador do `textblob` pode funcionar de maneira diferente com base no contexto de uso:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Saída:**
```
This is a test sentence
```

Lembre-se, enquanto os métodos `capitalize()` e `title()` são universalmente úteis, o uso de bibliotecas como `textblob` pode proporcionar flexibilidade adicional para aplicações específicas.
