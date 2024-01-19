---
title:                "Capitalizando uma string"
html_title:           "Python: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Capitalizar uma string significa tornar a primeira letra de cada palavra em maiúsculo. Programadores fazem isso para melhorar a estética do texto, tornar textos consistentes e facilitar a leitura.

## Como fazer:

No Python, temos o método title() para capitalizar strings. Aqui está um exemplo:

```Python
texto = 'programação em python é divertida.'
texto = texto.title()
print(texto)
```

A saída será:

```Python
'Programação Em Python É Divertida.'
```

## Mergulho profundo:

O método title() existe desde o início do Python. Foi uma maneira fácil de trazer a funcionalidade comum de processadores de texto para a linguagem de programação.

Uma alternativa ao método title() é o método capitalize(). No entanto, o método capitalize() torna a primeira letra da string em maiúsculo e todas as demais em minúsculo.

```Python
texto = 'python é DIVERTIDO.'
texto = texto.capitalize()
print(texto)
```

A saída será:

```Python
'Python é divertido.'
```

Em termos de implementação, esses métodos de string percorrem a string e alteram os caracteres para maiúsculas ou minúsculas conforme necessário.

## Veja também:

Para mais informações sobre strings no Python, consulte a documentação oficial do Python sobre strings em: https://docs.python.org/3/tutorial/introduction.html#strings