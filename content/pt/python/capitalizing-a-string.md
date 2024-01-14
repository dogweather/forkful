---
title:    "Python: Colocar em maiúsculas uma string"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Porque

Capitalizar uma string é um processo importante na programação Python, pois permite que as letras iniciais sejam maiúsculas e as demais minúsculas. Isso é útil para padronizar e organizar melhor os dados, tornando-os mais legíveis.

## Como Fazer

Para capitalizar uma string, podemos utilizar o método `capitalize()`. Veja um exemplo abaixo:

```
Python
string = "python é uma linguagem de programação popular"
capitalized_string = string.capitalize()
print(capitalized_string)
```

O resultado será: *"Python é uma linguagem de programação popular"*

É importante notar que apenas a primeira letra da string será capitalizada. Se houver alguma pontuação, números ou símbolos antes da primeira letra, eles serão mantidos como estão. Além disso, todas as letras após a primeira serão convertidas para minúsculas.

Outra forma de capitalizar uma string é utilizando o método `title()`, que irá capitalizar todas as palavras dentro da string.

```
Python
string = "programando em python"
capitalized_string = string.title()
print(capitalized_string)
```

O resultado será: *"Programando Em Python"*

## Mergulho Profundo

Além dos métodos `capitalize()` e `title()`, existem outras formas de capitalizar uma string em Python, como utilizando a função `upper()`, que converte todos os caracteres para maiúsculas, e a função `lower()`, que converte todos os caracteres para minúsculas.

Também é possível criar uma função personalizada para capitalizar uma string, utilizando os métodos `split()` e `join()`, que dividem a string em uma lista de palavras e depois as unem novamente, desta vez com a primeira letra de cada palavra capitalizada.

## Veja Também

- [Documentação oficial do Python sobre o método `capitalize()`](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Documentação oficial do Python sobre o método `title()`](https://docs.python.org/3/library/stdtypes.html#str.title)
- [Explicações sobre mais opções para capitalizar strings em Python](https://www.codecademy.com/learn/learn-python-3/modules/learn-python3-strings/cheatsheet)