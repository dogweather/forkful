---
title:                "Elixir: Encontrando o comprimento de uma string."
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa fundamental em qualquer linguagem de programação. Como programadores, frequentemente precisamos manipular e analisar strings em nossos projetos. É por isso que é importante entender como encontrar o comprimento de uma string em Elixir e usar essa habilidade em nossos códigos.

## Como fazer

Para encontrar o comprimento de uma string em Elixir, podemos usar a função `String.length/1`. Esta função recebe uma string como entrada e retorna o número de caracteres na string. Veja um exemplo abaixo:

```
Elixir
iex> String.length("Elixir")
6
```

Podemos também usar essa função para encontrar o comprimento de uma string armazenada em uma variável. Veja o exemplo abaixo:

```
Elixir
iex> str = "Esta é uma string"
iex> String.length(str)
17
```

Também podemos usar esta função para verificar se uma string está vazia. Se a string tiver comprimento 0, ela será considerada vazia. Veja o exemplo abaixo:

```
Elixir
iex> String.length("")
0
```

## Profundidade

Ao trabalhar com strings em Elixir, é importante entender que elas são imutáveis. Isso significa que uma string nunca será modificada e cada operação na string retornará uma nova string. Portanto, quando usamos a função `String.length/1`, não estamos alterando a string original, mas sim retornando o comprimento da string.

Também é importante notar que a função `String.length/1` é sensível a caracteres Unicode. Isso significa que ela contará corretamente o número de caracteres em uma string, mesmo que eles sejam caracteres Unicode de vários bytes.

## Veja também

- [Documentação da função `String.length/1`](https://hexdocs.pm/elixir/String.html#length/1)
- [Tutorial sobre strings em Elixir](https://elixirschool.com/pt/lessons/basics/strings/)
- [Guia de estilo de strings em Elixir](https://github.com/lexmag/elixir-style-guide#strings)