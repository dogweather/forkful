---
title:                "Maiúsculas de uma string"
html_title:           "Python: Maiúsculas de uma string"
simple_title:         "Maiúsculas de uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que capitalizar uma string?

Capitalizar uma string é o processo de modificar todas as letras de uma palavra ou frase para que as letras maiúsculas e minúsculas estejam em conformidade com as regras gramaticais do idioma. Programadores geralmente fazem isso para tornar o texto mais legível e padronizado.

## Como fazer:

```
# Exemplo 1: Capitalizar uma string usando o método capitalize()
texto = "python é uma linguagem de programação incrível!"
print(texto.capitalize())
# Saída: Python é uma linguagem de programação incrível!

# Exemplo 2: Capitalizar a primeira letra de cada palavra usando o método title()
texto = "python é uma linguagem de programação incrível!"
print(texto.title())
# Saída: Python É Uma Linguagem De Programação Incrível!

# Exemplo 3: Capitalizar todas as letras de uma string usando o método upper()
texto = "python é uma linguagem de programação incrível!"
print(texto.upper())
# Saída: PYTHON É UMA LINGUAGEM DE PROGRAMAÇÃO INCRÍVEL!
```

## Profundando um pouco mais:

Capitalizar strings é uma prática comum ao lidar com texto em programação. No passado, quando muitas linguagens de programação só permitiam a inserção de caracteres em maiúsculas, capitalizar strings era essencial para tornar o texto mais legível. Hoje em dia, existem diversas maneiras de capitalizar strings, como demonstrado nos exemplos acima. Alguns programadores podem preferir métodos mais específicos, dependendo do contexto em que estão trabalhando.

## Veja também:

- [Python Documentation: String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [GeeksforGeeks: String capitalize() method in Python](https://www.geeksforgeeks.org/python-string-capitalize/)
- [Real Python: Capitalizing Strings in Python](https://realpython.com/capitalize-strings-python/)