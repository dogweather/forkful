---
title:                "Encontrando o comprimento de uma string"
html_title:           "Elixir: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por que?
Encontrar o comprimento de uma string é uma operação comum em programação que consiste em determinar quantos caracteres existem dentro de uma string. Os programadores precisam determinar o comprimento de uma string para realizar diversas tarefas, como validar entrada do usuário, manipular dados ou formatar saída de dados.

## Como fazer:
```Elixir
String.length("Olá mundo") #=> 9
```
### Saída:
```
9
```

## Aprofundando-se:
A operação de encontrar o comprimento de uma string tem sido uma tarefa básica de programação desde os primeiros dias da criação de linguagens de programação. No entanto, dependendo da linguagem utilizada, pode haver diferenças na sua implementação.

No caso do Elixir, a função `String.length/1` retorna o número de caracteres na string fornecida como argumento. Outras linguagens, como C++, exigem que o programador crie uma função dedicada para encontrar o comprimento de uma string. Há também a possibilidade de usar a função `String.size/1`, que retorna o número de bytes na string, o que pode ser útil em casos de strings contendo caracteres Unicode.

## Veja também:
- [Documentação do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Outras formas de encontrar o comprimento de uma string em diferentes linguagens](https://en.wikipedia.org/wiki/String_length)