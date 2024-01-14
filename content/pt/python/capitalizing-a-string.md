---
title:                "Python: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Em programação, frequentemente precisamos modificar o formato de uma string para atender às nossas necessidades. A função de capitalização de strings é uma ferramenta útil para padronizar a capitalização de caracteres em uma string.

## Como capitalizar uma string

```Python
string = "programação é divertido!"
print(string.capitalize())

# Saída: Programação é divertido!
```

O método `capitalize()` irá transformar o primeiro caractere da string em maiúsculo e os demais caracteres em minúsculo. É importante notar que ele não modificará os caracteres especiais ou espaços em branco na string.

Além disso, podemos usar o método `upper()` para transformar todos os caracteres da string em maiúsculos.

```Python
string = "programação é divertido!"
print(string.upper())

# Saída: PROGRAMAÇÃO É DIVERTIDO!
```

E para transformar todos os caracteres em minúsculos, usamos o método `lower()`.

```Python
string = "PROGRAmAcãO é DIVertiDO!"
print(string.lower())

# Saída: programação é divertido!
```

## Mergulho Profundo

Ao usar o método `capitalize()`, é importante ter em mente que ele só irá alterar o primeiro caractere da string. Se a string já estiver com a primeira letra maiúscula, o método não fará nenhuma alteração. Além disso, como mencionado anteriormente, ele não alterará os caracteres especiais ou espaços em branco.

Por outro lado, o método `upper()` e `lower()` irão alterar todos os caracteres da string para maiúsculo ou minúsculo, independentemente do seu estado atual. É importante ter isso em mente ao escolher qual método usar para capitalizar uma string.

## Veja também

- [Documentação da função `capitalize()` em Python](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Como usar strings em Python](https://realpython.com/python-strings/)
- [Como formatar strings em Python](https://blog.finxter.com/python-string-formatting-the-definitive-guide/)