---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolação de Strings em Python

## Porquê e Para Quê?

A interpolação de strings é o processo de substituição de valores específicos em uma string. Programadores usam isso para inserir dinamicamente valores de variáveis em strings, tornando seus programas mais fexíveis e legíveis.

## Como Fazer:
O Python oferece várias maneiras de interpolar strings. Aqui, demonstramos o uso do método `format()` e as f-strings.

Usando o `format()`:

```Python
nome = "João"
print("Oi, me chamo {}".format(nome))
```
Saída: `Oi, me chamo João`

Utilizando f-strings:

```Python
nome = "Maria"
print(f"Oi, me chamo {nome}")
```
Saída: `Oi, me chamo Maria`

## Mergulho Profundo

A interpolação de string tem sido uma parte vital das linguagens de programação desde seu início. Em Python, o método `format()` foi introduzido na versão 2.6 para tornar a interpolação de strings mais eficiente. As f-strings, uma alternativa mais nova e preferida, foram adicionadas no Python 3.6.

Alternativas ao `format()` e f-strings são o operador `%` (muito como em C) e o método `Template` na biblioteca `string`. No entanto, são menos usadas por causa da facilidade e eficiência das f-strings.

A implementação de f-string é mais rápida que `format()`, isso porque as f-strings são expressas na linguagem de byte-código no momento da execução, fazendo com que as operações sejam mais rápidas.

## Veja Também

- Documentação do Python sobre f-strings: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
- Guia detalhado da Real Python sobre interpolação de string em Python: https://realpython.com/python-string-formatting/
- Postagem do blog de Dan Bader sobre porque preferir f-strings: https://dbader.org/blog/python-f-strings