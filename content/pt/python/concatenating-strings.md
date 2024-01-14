---
title:    "Python: Unindo cadeias de caracteres"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

Concatenar strings é uma técnica muito útil em programação, que permite unir diferentes pedaços de texto em uma única string. Isso pode ser útil para criar mensagens mais complexas, formatar textos de maneira específica ou até mesmo para criar URLs dinâmicas. 

## Como fazer?

Para concatenar strings em Python, podemos utilizar o operador de adição `+` ou o método `format()`. Veja abaixo dois exemplos de código, utilizando ambos os métodos:

```Python
# Utilizando o operador de adição
nome = "Maria"
sobrenome = "Silva"
print(nome + " " + sobrenome)

# Utilizando o método format()
idade = 26
cidade = "São Paulo"
print("Eu me chamo {} e tenho {} anos. Sou de {}.".format(nome, idade, cidade))
```

O código acima produzirá a seguinte saída:

```
Maria Silva
Eu me chamo Maria e tenho 26 anos. Sou de São Paulo.
```

## Aprofundando

Além desses dois métodos mais comuns, também podemos utilizar o método `.join()` para concatenar strings em Python. Ele funciona de maneira similar ao método `format()`, mas permite unir uma lista de strings utilizando um separador definido por nós. Veja o exemplo abaixo:

```Python
numeros = [1, 2, 3, 4, 5]
print(", ".join(str(num) for num in numeros))
```

A saída desse código será:

```
1, 2, 3, 4, 5
```

Note que utilizamos o método `str()` para converter os números da lista em strings antes de concatená-los.

## Veja também

- [Documentação oficial do Python sobre strings](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Tutorial sobre strings em Python](https://realpython.com/python-strings/)