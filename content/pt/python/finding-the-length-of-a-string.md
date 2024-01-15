---
title:                "Encontrando o comprimento de uma cadeia de caracteres"
html_title:           "Python: Encontrando o comprimento de uma cadeia de caracteres"
simple_title:         "Encontrando o comprimento de uma cadeia de caracteres"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por Que

Você já se perguntou como os programas conseguem saber quantos caracteres têm em uma palavra ou frase? Saber o tamanho de uma string é uma habilidade essencial para qualquer programador, e neste artigo vamos te mostrar como fazer isso usando Python.

## Como Fazer

Para encontrar o tamanho de uma string em Python, usamos a função `len()`. Por exemplo, se tivermos a string "Olá, mundo", podemos usar a função da seguinte maneira:

```Python
print(len("Olá, mundo"))
```

O resultado dessa função será 11, já que há 11 caracteres, incluindo espaços, na frase "Olá, mundo".

Você também pode utilizar a função `len()` para encontrar o tamanho de uma variável que contenha uma string. Por exemplo:

```Python
mensagem = "Python é incrível!"
print(len(mensagem))
```

O resultado será novamente 11, pois a variável `mensagem` contém uma string com 11 caracteres.

## Deep Dive

Ao usar a função `len()`, é importante ter em mente que ela conta todos os caracteres da string, incluindo espaços em branco e caracteres especiais como pontuação. Além disso, a função também pode ser usada para encontrar o tamanho de outros tipos de dados, como listas e dicionários.

Outra coisa importante a se notar é que, em Python, as strings são imutáveis, o que significa que não podemos alterar diretamente a string original. Isso significa que, se tentarmos atribuir um novo valor a um determinado índice em uma string, receberemos um erro.

Por exemplo, se tentarmos executar o seguinte código:

```Python
mensagem = "Python é incrível!"
mensagem[0] = "J"
```

Receberemos um erro informando que a string não pode ser modificada. Para contornar isso, podemos usar a concatenação de strings para criar uma nova string com o valor atualizado.

## Veja Também

- [Tutorial de Introdução à Programação em Python](https://www.learnpython.org/pt/)
- [Documentação oficial do Python](https://docs.python.org/pt-br/3/index.html)
- [Artigo do Real Python sobre strings em Python](https://realpython.com/python-strings/)