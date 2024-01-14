---
title:                "Gleam: Extraindo subcadeias"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings é uma tarefa comum em muitos projetos de programação e pode ser útil por diversos motivos. Algumas possibilidades incluem extrair informações específicas de uma string, como um nome ou um número de telefone, ou realizar manipulações em uma parte específica de uma string.

## Como extrair substrings em Gleam

Extrair substrings em Gleam é uma tarefa simples e pode ser feita usando a função `substring` da biblioteca padrão. Primeiro, devemos declarar a string original que desejamos extrair a substring:

```Gleam
let input = "Este é um exemplo de string"
```

Em seguida, podemos usar a função `substring` para extrair uma substring a partir da posição inicial e com o comprimento desejado:

```Gleam
let output = input.substring(5, 10)
```

O resultado será a substring "é um exe", pois começando do quinto caractere ("é") e contando 10 caracteres, chegamos até o décimo quinto caractere ("e").

## Uma olhada mais profunda

Existem algumas coisas a serem consideradas ao extrair substrings em Gleam. Primeiro, a posição inicial da substring deve estar dentro dos limites da string original. Caso contrário, a função `substring` retornará um erro. Além disso, o comprimento da substring deve ser um inteiro positivo e também deve ser uma posição dentro dos limites da string original.

Outro ponto importante é que a função `substring` retorna uma nova string, ou seja, a string original não é modificada. Sempre devemos atribuir o resultado da função a uma nova variável.

## Veja também

- Documentação da função `substring` em Gleam: https://gleam.run/core/string.html#substring
- Tutorial de programação em Gleam: https://gleam.run/book/getting-started.html
- Comunidade Gleam no Discord: https://discord.gg/J6d3KD5A