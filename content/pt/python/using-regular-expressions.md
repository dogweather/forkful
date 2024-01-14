---
title:                "Python: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares

Se você já se encontrou em uma situação em que precisava encontrar ou manipular determinado padrão de texto em um arquivo, provavelmente já se deparou com uma tarefa quase impossível de ser feita manualmente. É aí que entram as expressões regulares. Elas são uma ferramenta poderosa para buscar e manipular padrões de texto em documentos. Com elas, é possível economizar tempo e aumentar a eficiência na programação.

## Como utilizar expressões regulares em Python

As expressões regulares são padrões de sequência de caracteres usados para pesquisar e manipular strings. Para utilizar expressões regulares em Python, é necessário importar o módulo `re`. Em seguida, é possível utilizar funções como `search()`, `findall()` e `sub()` para encontrar e manipular padrões de texto.

Vamos ver um exemplo de como encontrar um padrão específico em uma string usando o `search()`:

```
import re

texto = "Hoje é um lindo dia para programar!"

padrao = "lindo"

resultado = re.search(padrao, texto)

if resultado:
  print("Padrão encontrado!")
else:
  print("Padrão não encontrado!")
```

A saída desse código será "Padrão encontrado!", pois o padrão "lindo" foi encontrado na string "Hoje é um lindo dia para programar!".

Outra função útil é o `findall()`, que retorna uma lista com todas as ocorrências do padrão em uma string. E o `sub()`, que substitui todas as ocorrências do padrão por uma string desejada.

## Aprofundando-se em expressões regulares

Para utilizar as expressões regulares de forma eficiente, é importante conhecer os padrões de sintaxe utilizados para criar essas expressões. Alguns exemplos são `.` para representar qualquer caractere, `*` para indicar que o caractere anterior pode se repetir várias vezes e `[]` para criar uma classe de caracteres. Além disso, é possível utilizar metacaracteres, que são caracteres especiais para buscar tipos específicos de padrões.

O uso de expressões regulares também pode ser combinado com outras ferramentas em Python, como loops e listas. Isso permite criar lógicas complexas para buscar e manipular strings em arquivos.

É importante ressaltar que, apesar de serem bastante úteis, as expressões regulares podem ser complicadas e exigem um bom conhecimento de programação. Portanto, é necessário praticar bastante e sempre consultar a documentação oficial do Python para desenvolver suas habilidades nesta área.

## Veja também

- [Documentação oficial do módulo `re` em Python](https://docs.python.org/3/library/re.html)
- [Livro "Introdução à Expressões Regulares" - Rafael Nepô](https://raccoon.ninja/pt/dev-pt/livro-introducao-a-expressoes-regulares/)