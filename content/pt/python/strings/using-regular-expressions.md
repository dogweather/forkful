---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:19.779729-07:00
description: "Como usar: Usar regex em Python envolve o m\xF3dulo `re`, que fornece\
  \ um conjunto de fun\xE7\xF5es para processar texto usando express\xF5es regulares."
lastmod: '2024-04-05T21:53:46.465830-06:00'
model: gpt-4-0125-preview
summary: "Usar regex em Python envolve o m\xF3dulo `re`, que fornece um conjunto de\
  \ fun\xE7\xF5es para processar texto usando express\xF5es regulares."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como usar:
Usar regex em Python envolve o módulo `re`, que fornece um conjunto de funções para processar texto usando expressões regulares.

### Correspondência de Padrão Básica
Para procurar um padrão em uma string, use `re.search()`. Ele retorna um objeto de correspondência quando o padrão é encontrado, caso contrário `None`.
```python
import re

text = "Aprenda a programar em Python"
match = re.search("Python", text)
if match:
    print("Padrão encontrado!")
else:
    print("Padrão não encontrado.")
```
Saída:
```
Padrão encontrado!
```

### Compilando Expressões Regulares
Para uso repetido do mesmo padrão, compile-o primeiro com `re.compile()` para melhor desempenho.
```python
pattern = re.compile("Python")
match = pattern.search("Aprenda a programar em Python")
if match:
    print("Padrão compilado encontrado!")
```
Saída:
```
Padrão compilado encontrado!
```

### Dividindo Strings
Para dividir uma string em cada correspondência de um padrão regex, use `re.split()`.
```python
result = re.split("\s", "Python é divertido")
print(result)
```
Saída:
```
['Python', 'é', 'divertido']
```

### Encontrando Todas as Correspondências
Para encontrar todas as ocorrências não sobrepostas de um padrão, use `re.findall()`.
```python
matches = re.findall("n", "Programação em Python")
print(matches)
```
Saída:
```
['n', 'n']
```

### Substituindo Texto
Use `re.sub()` para substituir ocorrências de um padrão por uma nova string.
```python
replaced_text = re.sub("divertido", "incrível", "Python é divertido")
print(replaced_text)
```
Saída:
```
Python é incrível
```

### Bibliotecas de Terceiros
Embora o módulo `re` embutido do Python seja poderoso, bibliotecas de terceiros como `regex` oferecem mais recursos e desempenho aprimorado. Para usar `regex`, instale-o via pip (`pip install regex`) e importe-o em seu código.

```python
import regex

text = "Aprendendo Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"Versão encontrada: {match.group(1)}")
```
Saída:
```
Versão encontrada: 3.8
```
