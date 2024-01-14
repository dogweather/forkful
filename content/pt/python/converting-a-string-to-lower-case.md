---
title:                "Python: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Por que converter uma string em letras minúsculas?

Muitas vezes, ao manipular strings em Python, é necessário padronizar o formato para facilitar a comparação e o processamento dos dados. Converter uma string para letras minúsculas pode ser útil para garantir que todas as letras sejam tratadas da mesma forma, independentemente de estarem em maiúsculas ou minúsculas.

##Como fazer?

Para converter uma string para letras minúsculas em Python, podemos usar o método `lower()`. Veja um exemplo de código abaixo:

```Python
texto = "PYTHON É DIVERTIDO!"
print(texto.lower())
```

O resultado deste código seria `python é divertido!`. Note que todas as letras foram convertidas para minúsculas.

Podemos também aplicar o método `lower()` em uma variável que já contém uma string, como no exemplo abaixo:

```Python
nome = "JOÃO"
nome = nome.lower()
print(nome)
```

Neste caso, a variável `nome` teria como valor `joão` após a conversão para letras minúsculas.

##Aprofundando-se

Além do método `lower()`, também é possível utilizar a função `casefold()` para converter uma string para letras minúsculas em Python. A principal diferença entre os dois é que `casefold()` leva em consideração caracteres acentuados e de outras línguas, enquanto `lower()` não os altera.

Também é importante mencionar que a conversão para letras minúsculas em Python respeita as regras de acentuação e capitalização da língua em que o código está sendo executado. Ou seja, se o seu sistema estiver configurado para português, a conversão para minúsculas também será feita de acordo com as regras do português.

##Veja também

A seguir, estão alguns links úteis para aprender mais sobre a conversão de strings em Python:

- Documentação oficial do Python sobre o método `lower()`: https://docs.python.org/3/library/stdtypes.html#str.lower
- Documentação oficial do Python sobre a função `casefold()`: https://docs.python.org/3/library/stdtypes.html#str.casefold
- Artigo do site Real Python sobre métodos de manipulação de strings em Python: https://realpython.com/python-strings/