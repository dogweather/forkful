---
title:                "Python: Extraindo substrings"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que
Extração de substrings é uma habilidade fundamental para programadores Python, pois permite manipular e extrair partes específicas de uma string. Isso pode ser útil em diversas situações, como tratamento de dados e criação de algoritmos.

## Como Fazer
Para extrair uma substring em Python, é preciso utilizar a sintaxe de "slicing", que consiste em definir um intervalo de índices para capturar a parte desejada da string. Por exemplo, se tivermos a string "Python é uma linguagem de programação", podemos extrair apenas a palavra "linguagem" utilizando o seguinte código:

```Python
string = "Python é uma linguagem de programação"

substring = string[13:22]

print(substring) # output: linguagem
```

No código acima, especificamos o intervalo de índices do 13 ao 22, que corresponde à posição inicial e final da palavra "linguagem". É importante destacar que o primeiro índice é sempre incluído na substring, enquanto o último índice não é.

É possível também utilizar índices negativos para extrair substrings a partir do final da string. Por exemplo, se quisermos extrair apenas a última palavra da string acima, podemos fazer o seguinte:

```Python
substring = string[-13:-1]

print(substring) # output: programação
```

Além disso, é possível utilizar a sintaxe de "slicing" para extrair caracteres específicos de cada palavra. Por exemplo, se quisermos obter apenas as letras "o" de cada palavra da string acima, podemos utilizar o seguinte código:

```Python
string = "Python é uma linguagem de programação"

substring = string[1::2]

print(substring) # output: yhnumaimegemd orgaç
```

No exemplo acima, utilizamos o operador de passo (2) para pular um caractere a cada iteração, resultando apenas nas letras "o" de cada palavra.

## Mergulho Profundo
A sintaxe de "slicing" pode ser utilizada em diferentes formas para extrair substrings em Python. Além disso, é possível combinar o uso de índices com métodos de manipulação de strings, como o método `.split()` que divide uma string em uma lista de substrings. Isso permite uma maior versatilidade e flexibilidade ao manipular e extrair informações de strings em Python.

## Veja Também
- [Documentação Oficial do Python](https://docs.python.org/3/tutorial/introduction.html)
- [Artigo sobre "slicing" em strings](https://www.programiz.com/python-programming/methods/string/slice)