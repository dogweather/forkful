---
title:                "Python: Capitalizando uma string"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Python?

Capitalizar uma string em Python é uma tarefa comum em muitos projetos de programação. Isso ocorre porque a capitalização é importante para padronizar e melhorar a legibilidade do código. Além disso, alguns métodos e funções em Python só funcionam corretamente com strings capitalizadas. Neste artigo, vamos explorar como capitalizar uma string em Python e por que isso é importante.

## Como capitalizar uma string em Python

Capitalizar uma string em Python é um processo simples que pode ser feito de várias maneiras. Aqui estão dois métodos comuns usando a função `capitalize()` e o método `upper()`:

```Python
texto = "este é um exemplo de string capitalizada"
print(texto.capitalize())
# Output: Este é um exemplo de string capitalizada

texto2 = "este é outro exemplo de string capitalizada"
print(texto2.upper())
# Output: ESTE É OUTRO EXEMPLO DE STRING CAPITALIZADA
```

Além desses métodos, também é possível usar a função `title()` para capitalizar automaticamente cada palavra em uma string, ou seja, o primeiro caractere de cada palavra será maiúsculo:

```Python
texto = "este é um exemplo de string capitalizada"
print(texto.title())
# Output: Este É Um Exemplo De String Capitalizada
```

## Profundidade no processo de capitalização de strings

Ao capitalizar uma string, é importante entender que as letras maiúsculas e minúsculas são diferentes em Python. Isso significa que a string "String" é diferente da string "string". Isso pode afetar o resultado final da capitalização. Além disso, existem funções e métodos que podem ser usados para verificar se uma string está ou não capitalizada, como `isupper()` e `islower()`.

Também é possível capitalizar apenas a primeira letra de cada palavra em uma string, mantendo o restante das letras minúsculas, usando a função `title()` com o método `split()` e `join()`:

```Python
texto = "este é um exemplo de string capitalizada"
capitalizado = " ".join(word.capitalize() for word in texto.split())
print(capitalizado)
# Output: Este É Um Exemplo De String Capitalizada
```

Como você pode ver, o processo de capitalização de strings é fácil e pode ser feito de várias maneiras. No entanto, é importante ter em mente que as letras maiúsculas e minúsculas têm significados diferentes em Python e isso pode afetar o resultado da capitalização.

## Veja também

Aqui estão alguns recursos úteis para aprender mais sobre capitalização em Python:

- [Documentação oficial do Python sobre strings](https://docs.python.org/3/library/string.html)
- [Artigo da Real Python sobre strings em Python](https://realpython.com/python-strings/)
- [Tutorial do W3Schools sobre manipulação de strings em Python](https://www.w3schools.com/python/python_strings.asp)