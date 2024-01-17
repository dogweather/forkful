---
title:                "Utilizando expressões regulares"
html_title:           "Python: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que e por que?
Regular expressions, também conhecidas como regex, são uma poderosa ferramenta usada em programação para encontrar e manipular padrões de texto. Isso significa que, com elas, podemos procurar por certos padrões em strings de texto e fazer alterações se necessário. Programadores usam regular expressions para automatizar tarefas de manipulação de texto que seriam tediosas e demoradas para serem feitas manualmente.

## Como usar:
```Python
import re

texto = "Olá, eu sou um texto de exemplo!"
padrao = "texto"

match = re.search(padrao, texto) #procura pelo padrão no texto

print(match.group()) #imprime o resultado
```
  Output: "texto"

## Aprofundando:
As expressões regulares foram inventadas pelo matemático americano Stephen Kleene na década de 1950. Desde então, elas se tornaram uma ferramenta fundamental em programação. Em Python, podemos usar regular expressions através do módulo "re". No entanto, existem outras opções, como a biblioteca "regex" ou até mesmo funções nativas da linguagem, como o "split" e "replace". A principal diferença entre essas alternativas é a sintaxe utilizada.

## Veja também:
- Documentação oficial do módulo "re": https://docs.python.org/3/library/re.html
- Tutorial de Regular Expressions em Python: https://www.w3schools.com/python/python_regex.asp
- Outras bibliotecas úteis para manipulação de texto em Python: https://realpython.com/python-string-manipulation/#python-text-manipulation-tools