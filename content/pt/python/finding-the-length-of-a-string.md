---
title:                "Python: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que: Entendendo a Importância de Encontrar o Comprimento de uma String

Ao trabalhar com programação em Python, é muito comum lidar com strings - que são conjuntos de caracteres. Para poder manipular essas strings de forma eficiente, é importante saber o seu comprimento. Encontrar o comprimento de uma string pode ser útil em diversas situações, como por exemplo, para validar o input do usuário ou para fazer operações matemáticas com esses valores. Neste artigo, exploraremos como encontrar o comprimento de uma string em Python.

## Como Fazer: Encontrando o Comprimento de uma String em Python

Em Python, podemos usar a função `len()` para encontrar o comprimento de uma string. Esta função aceita um argumento, que deve ser uma string, e retorna o número de caracteres presentes nessa string. Veja um exemplo de código abaixo:

```python
frase = "Olá, mundo!"
comprimento = len(frase)
print(comprimento)
```
**Saída:**
```
13
```

Neste exemplo, definimos uma variável chamada `frase` que contém a string "Olá, mundo!" e em seguida, usamos a função `len()` para encontrar o seu comprimento e armazenamos o resultado na variável `comprimento`. Por fim, imprimimos esse valor na tela.

Além disso, é importante ressaltar que a função `len()` também pode ser usada para encontrar o comprimento de outros tipos de dados, como listas e dicionários. Veja um exemplo abaixo:

```python
lista = [1, 2, 3, 4, 5]
comprimento_lista = len(lista)
print(comprimento_lista)
```
**Saída:**
```
5
```

## Mergulho Profundo: Entendendo como a Função `len()` Funciona

Agora que já sabemos como usar a função `len()` para encontrar o comprimento de uma string em Python, vamos entender um pouco mais sobre como ela funciona. Em Python, cada tipo de dado é representado por uma classe, e essas classes possuem diferentes métodos e propriedades. No caso das strings, a classe `str` possui o método `__len__()`, que é responsável por retornar o comprimento dessa string. Isso significa que quando usamos a função `len()`, na verdade estamos acessando esse método por trás dos panos.

Vale ressaltar que a função `len()` só funciona com objetos que possuem o método `__len__()`, o que inclui strings, listas, tuplas, dicionários, entre outros.

## Veja Também

- [Documentação oficial do Python sobre a função len()](https://docs.python.org/3/library/functions.html#len)
- [Tutorial sobre strings em Python](https://realpython.com/python-strings/)
- [Artigo sobre as diferentes formas de encontrar o comprimento de uma string](https://www.techbeamers.com/python-string-length/#Finding_Length_of_String_in_Python_Language)