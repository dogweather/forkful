---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Extrair substrings é o processo de selecionar e manipular partes específicas de strings em Python. Fazemos isso para manipular dados, como na análise de texte e extração de informação útil de strings grandes.

## Como Fazer:

Aqui, vou mostrar como extrair substrings por slicing e usando o método find.

```python
texto = "Olá, sou um programador Python!"

# Extrair substrings por slicing
print(texto[0:4])  # Saída: "Olá,"

# Usar o método find para encontrar a posição de uma substring
posicao = texto.find("Python")
print(texto[posicao:])  # Saída: "Python!"
```

## Aprofundando:

A opção de slicing foi adicionada no início do Python, tornando mais fácil para os programadores manipular strings. No entanto, encontrar uma substring específica era sempre complicado - até que o método `find` foi introduzido.

As alternativas para a extração de substrings incluem o uso do método split(), regex ou bibliotecas como pandas quando trabalhando com dados maiores. 

Detalhes de implementação: O slicing em Python retorna uma nova string e não modifica a original. No entanto, o método `find` retorna uma posição, que deve ser usado com o slicing para extrair a substring.

## Veja Também:

1. Documentação oficial do Python em strings: https://docs.python.org/pt-br/3/tutorial/introduction.html#strings
2. Tutorial no Python para manipulação de strings: https://realpython.com/python-strings/
3. Um guia no slicing de strings do Python: https://www.digitalocean.com/community/tutorials/how-to-index-and-slice-strings-in-python-3