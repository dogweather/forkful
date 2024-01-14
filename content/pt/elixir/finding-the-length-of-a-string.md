---
title:    "Elixir: Encontrando o comprimento de uma string"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Saber como obter o tamanho de uma string é uma habilidade básica e essencial para qualquer programador. Além disso, é uma tarefa comum em muitas aplicações, o que torna importante entender como ela funciona em Elixir.

## Como fazer

A linguagem de programação Elixir possui uma função embutida para encontrar o tamanho de uma string. Para usá-la, basta chamar a função `String.length()` seguida da string desejada dentro de parênteses. Veja um exemplo:

```Elixir
nome = "Maria"

String.length(nome)
```
A saída desse código seria `5`, pois a string "Maria" possui 5 caracteres. Além disso, essa função também funciona com qualquer outra string, independente do seu tamanho ou conteúdo.

## Mergulho profundo

Para entender melhor como a função `String.length()` funciona, precisamos saber um pouco mais sobre strings em Elixir. Em primeiro lugar, é importante lembrar que os caracteres em Elixir são representados por valores numéricos de 8 bits, ou seja, cada caractere possui um código único e específico.

Quando usamos a função `String.length()`, ela retorna a quantidade de caracteres em uma string, mas na verdade ela está contando a quantidade de caracteres em bytes. Por exemplo, a string "çáé" possui 3 caracteres, mas a função `String.length()` retornaria 5, pois cada caractere ocupa 2 bytes.

Porém, é importante lembrar que nem todos os caracteres são representados por 2 bytes. Alguns caracteres especiais, como emojis, possuem códigos maiores e podem ocupar mais bytes. Por isso, é sempre bom ter isso em mente ao trabalhar com strings em Elixir.

## Veja também

- [Documentação oficial do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Tutorial de Elixir do Alura](https://www.alura.com.br/conteudo/elixir)
- [Comunidade de Elixir no Brasil](https://elixirbrasil.org/)