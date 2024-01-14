---
title:    "Python: Extraindo substrings"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como extrair partes específicas de uma string em Python? Bem, essa habilidade é extremamente útil ao trabalhar com dados, processamento de texto e até mesmo na criação de aplicativos. Neste artigo, vamos explorar como é possível extrair substrings em Python e por que isso é importante.

## Como fazer

Extrair substrings em Python é bastante simples. Tudo o que você precisa é da função `slice` e o uso do operador de indexação `[]`. Por exemplo, se tivermos a string "Python é incrível", podemos usar a sintaxe `string[início:fim]` para extrair a palavra "incrível". Veja o código abaixo para entender melhor:

```Python
frase = "Python é incrível"
print(frase[6:13]) 
```

A saída do código acima será "incrível", pois estamos selecionando os caracteres da 6ª posição até a 12ª posição (a posição final fica de fora). E se quisermos extrair apenas a palavra "Python" da string? Podemos fazer isso usando índices negativos, indicando onde a substring começa a partir do final da string:

```Python
frase = "Python é incrível"
print(frase[:-9]) 
```

Neste exemplo, a saída será "Python", pois estamos selecionando todos os caracteres da string, exceto os últimos 9. Experimente mudar os números e veja como o resultado muda.

## Mergulho Profundo

Além da sintaxe básica mostrada acima, existem muitas outras maneiras de extrair substrings em Python. Por exemplo, você pode usar o método `split` para dividir a string em uma lista de substrings, usando um caractere específico como separador. Além disso, você pode usar as funções `strip` e `replace` para remover caracteres desnecessários da string antes de extrair as substrings.

É importante lembrar que os índices em Python começam em 0, e que o último índice é sempre -1. Além disso, a função `slice` também aceita um terceiro argumento opcional para controlar o "passo" da extração de substrings.

Com todos esses recursos e mais um pouco de prática, você certamente estará apto a extrair substrings em Python com facilidade.

## Veja também

- [Documentação oficial do Python sobre slicing de strings](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Artigo da Real Python sobre slicing em Python](https://realpython.com/python-slices/)
- [Tutorial da Codecademy sobre manipulação de strings em Python](https://www.codecademy.com/courses/learn-python-3/lessons/python-strings/exercises/string-manipulation)