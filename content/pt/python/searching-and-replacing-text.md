---
title:    "Python: Buscando e substituindo texto"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que usar busca e substituição de texto?

A busca e substituição de texto é uma função muito útil na programação que permite encontrar e substituir um determinado texto por outro. Isso pode ser útil para corrigir erros, alterar dados ou até mesmo automatizar tarefas repetitivas.

## Como fazer isso em Python

Para realizar uma busca e substituição em Python, podemos usar a função `replace` em strings. Veja um exemplo:

```Python
texto = "Olá, mundo!"
novo_texto = texto.replace("mundo", "amigos")
print(novo_texto)
```

Esse código irá imprimir "Olá, amigos!", substituindo a palavra "mundo" por "amigos".

## Aprofundando na busca e substituição de texto

Além da função `replace`, existem outras maneiras de realizar busca e substituição em Python. Por exemplo, podemos usar expressões regulares para encontrar padrões específicos em um texto e substituí-los. Também podemos usar a biblioteca `re` para isso. Veja um exemplo:

```Python
import re
texto = "Hoje é dia 20/05/2021"
novo_texto = re.sub(r"(\d{2})/(\d{2})/(\d{4})", r"\2-\1-\3", texto)
print(novo_texto)
```

Esse código irá imprimir "Hoje é dia 2021-05-20", substituindo a data no formato "dd/mm/yyyy" por "yyyy-mm-dd".

## Veja também

- [Documentação oficial do Python sobre a função `replace`](https://docs.python.org/3/library/string.html#string.replace)
- [Tutorial sobre expressões regulares em Python](https://www.w3schools.com/python/python_regex.asp)
- [Documentação oficial da biblioteca `re`](https://docs.python.org/3/library/re.html)