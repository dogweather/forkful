---
title:                "Interpolação de uma string"
html_title:           "Python: Interpolação de uma string"
simple_title:         "Interpolação de uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores utilizam a interpolação de strings?

A interpolação de strings é uma técnica muito útil para trabalhar com strings em Python. Basicamente, ela nos permite inserir variáveis em uma string de forma simples e rápida. Os programadores utilizam a interpolação de strings para economizar tempo e deixar o código mais legível.

## Como fazer:

Veja abaixo um exemplo de como utilizar a interpolação de strings em Python:

```Python
nome = "João"
idade = 25

# Utilizando f-strings
print(f"Olá, meu nome é {nome} e tenho {idade} anos.")

# Utilizando o método format()
print("Olá, meu nome é {} e tenho {} anos.".format(nome, idade))

# Utilizando a antiga formatação com o %s e %d
print("Olá, meu nome é %s e tenho %d anos." %(nome, idade))
```

Saída:
```
Olá, meu nome é João e tenho 25 anos.
Olá, meu nome é João e tenho 25 anos.
Olá, meu nome é João e tenho 25 anos.
```

## Mergulho profundo:

A interpolação de strings é uma técnica bastante antiga e foi introduzida na linguagem de programação Python a partir da versão 3.6. Antes disso, os programadores utilizavam métodos como `format()` e a formatação com o `%s` e `%d`. 

Alguns programadores preferem utilizar a interpolação de strings ao invés de concatenar strings com as variáveis, pois ela deixa o código mais legível e organizado. Além disso, as f-strings são mais rápidas do que o método `format()`, pois não é necessário percorrer a string em busca de espaços reservados para substituir por variáveis.

A interpolação de strings também é amplamente utilizada em outras linguagens de programação como JavaScript, Ruby e C#.

## Veja também:

- Documentação oficial do Python sobre f-strings: https://docs.python.org/3/whatsnew/3.6.html#pep-498-formatted-string-literals
- Mais informações sobre interpolação de strings em Python: https://realpython.com/python-f-strings/