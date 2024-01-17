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

## O que é e por que os programadores extraem substrings?

A extração de substrings é uma técnica comum na programação que envolve a obtenção de uma parte específica de uma string ou texto maior. Os programadores fazem isso por várias razões, como separar informações importantes de uma string grande, manipular dados de forma mais eficiente ou simplificar tarefas.

## Como fazer:

Extrair substrings em Python é bastante simples e pode ser feito usando o método `.split()` ou usando índices de strings. Vejamos alguns exemplos:

```
#Exemplo 1: Usando .split()
texto = "Olá, Portugal!"
separado = texto.split(",") #separa a string em duas partes usando a vírgula como delimitador
print(separado) #saída: ['Olá', ' Portugal!']

#Exemplo 2: Usando índices
texto = "Olá, Brasil!"
print(texto[0:4]) #pega os primeiros quatro caracteres da string, 'Olá'
print(texto[-1]) #pega o último caractere da string, '!'

```

## Detalhando melhor:

A extração de substrings tem sido usada na programação há muito tempo como uma forma de manipular e processar dados de forma mais eficiente. Antes do surgimento da linguagem Python, era comum utilizar técnicas como `substring()` em outras linguagens de programação como C++ e Java. No entanto, a simplicidade e a flexibilidade da sintaxe do Python tornaram a extração de substrings ainda mais fácil e acessível.

Além do método `.split()` e índices de strings, existem outras abordagens de extração de substrings em Python, como o `re` (módulo de expressões regulares) e o `find()` (método de procura em strings). Cada um desses métodos tem suas próprias vantagens e desvantagens, tornando a extração de substrings uma técnica versátil que pode ser adaptada para diferentes situações.

## Veja também:

- Documentação do método `.split()` em Python: https://docs.python.org/3/library/stdtypes.html#str.split
- Tutorial sobre índices de strings em Python: https://www.w3schools.com/python/python_strings_slicing.asp
- Exemplos de uso de expressões regulares em Python: https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial