---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Obter o comprimento de uma string significa descobrir quantos caracteres existem na string. Os programadores fazem isso para manipular dados de texto eficientemente, como condicionar tarefas baseadas no tamanho de uma entrada de texto.

## Como Fazer:

Para descobrir o comprimento de uma string em Python, usamos a função integrada `len()`. Aqui está um exemplo:
```Python
mensagem = "Olá, Mundo"
comprimento = len(mensagem)

print(comprimento)
```
Resultado esperado:
```Python
10
```
Nesse código, a mensagem `"Olá, Mundo"` contém 10 caracteres, incluindo a vírgula e o espaço. 

## Mergulho Profundo:

Python faz a contagem de caracteres de uma maneira principalmente linear - isto é, leva aproximadamente o mesmo tempo para contar uma string com 10 caracteres ou 10 mil caracteres. Contudo, outros idiomas podem lidar com essa tarefa de maneira diferente.

Uma alternativa ao uso do `len()` seria iterar através da string com um loop, embora isso seja menos eficiente.

```Python
mensagem = "Olá, Mundo"
comprimento = 0

for char in mensagem:
    comprimento += 1

print(comprimento)
```
Saída do código:
```Python
10
```

No entanto, o uso do `len()` é considerado o 'padrão Python'. Ele é usado não apenas para strings, mas em qualquer objeto iterável, levando este conceito de versatibilidade da linguagem Python.

## Ver Também:

Para mais informações sobre manipulação de strings em Python, você pode verificar os seguintes links:

1. [Python Official Docs on String methods](https://docs.python.org/pt-br/3/library/stdtypes.html#string-methods)
2. [A comprehensive guide to Python String](https://realpython.com/python-strings/)
3. [Python len() function](https://www.programiz.com/python-programming/methods/built-in/len)