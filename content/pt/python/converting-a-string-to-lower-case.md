---
title:                "Convertendo uma string para minúsculas"
html_title:           "Python: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Converter uma string para letras minúsculas é um processo em que todas as letras em uma string são alteradas para sua forma minúscula. Os programadores muitas vezes o fazem para garantir que a entrada do usuário não seja sensível a maiúsculas e minúsculas, o que pode causar erros ao comparar strings.

## Como fazer:

```Python
my_string = "Hello, WORLD!"
print(my_string.lower())
```

**Saída:**

```Python
hello, world!
```

## Aprofundando:

Converter strings para letras minúsculas tem sido uma prática comum desde os primeiros dias da programação. O motivo por trás disso é que computadores geralmente reconhecem letras maiúsculas e minúsculas como diferentes. Portanto, ao converter uma string inteiramente para letras minúsculas, os programadores podem evitar problemas ao comparar strings.

Existem algumas alternativas ao usar o método `.lower()` em Python, como usar o método `.casefold()`, que leva em conta caracteres especiais de diferentes idiomas. Além disso, os programadores também podem usar funções como `.capitalize()` e `.title()` para converter strings para outros formatos.

A implementação do método `.lower()` em Python tem um desempenho muito rápido e não requer nenhuma importação ou instalação adicional. Isso torna uma opção conveniente e eficiente para converter strings em letras minúsculas.

## Veja também:

- [Documentação oficial do método `.lower()` em Python] (https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Método `.casefold()` em Python] (https://www.geeksforgeeks.org/python-string-casefold-method/)
- [Alternativas para converter strings em Python] (https://stackoverflow.com/questions/319426/how-do-i-lowercase-a-string-in-python)