---
title:                "Extraindo Substrings"
html_title:           "Python: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extração de substrings é uma técnica muito útil para manipular e obter informações específicas de strings em Python. É especialmente útil para tarefas como análise de dados e processamento de linguagem natural.

## Como fazer

```Python
# Definindo uma string
texto = "Este é um texto de exemplo"

# Usando o método "find" para encontrar a posição da palavra "texto"
posicao = texto.find("texto")

# Imprimindo a posição da palavra
print(posicao)

# Saída: 12 (a palavra "texto" começa na posição 12 da string)

# Usando a posição para extrair a substring que contém a palavra "texto"
subtexto = texto[posicao:]

# Imprimindo a substring
print(subtexto)

# Saída: texto de exemplo
```

Outra maneira de extrair substrings é usando colchetes com um número que representa a posição da letra que você deseja extrair.

```Python
# Definindo uma string
frase = "Python é uma linguagem de programação"

# Extraindo a segunda palavra
segunda_palavra = frase[7:10]

# Imprimindo a segunda palavra
print(segunda_palavra)

# Saída: é
```

## Mergulho profundo

A extração de substrings em Python é uma tarefa simples, mas é importante ter algumas coisas em mente. Primeiramente, é importante notar que a contagem de posições começa em 0, ou seja, a primeira posição é representada por 0 e não por 1. Além disso, o método `find` só retorna a primeira ocorrência da palavra na string.

Outra coisa importante a notar é que o número entre os colchetes ao extrair uma substring representa o índice do caractere. Por exemplo, na string "Python", "P" está na posição 0, "y" está na posição 1 e assim por diante.

## Veja também

- [Documentação oficial do Python sobre `find`](https://docs.python.org/3/library/stdtypes.html#str.find)
- [Explicação detalhada sobre indexação de strings em Python](https://realpython.com/python-strings/#indexing-and-cutting-strings)