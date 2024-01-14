---
title:    "Elixir: Encontrando o comprimento de uma string."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que

Se você está começando a aprender Elixir, uma das primeiras coisas que você provavelmente vai querer saber é como encontrar o comprimento de uma string. Saber o tamanho de uma string é um conhecimento fundamental que pode ser aplicado em muitas situações de programação. Neste post, vamos explorar como fazer isso em Elixir.

## Como fazer

A maneira mais simples de encontrar o comprimento de uma string em Elixir é usando a função `length/1`. Veja um exemplo abaixo:

```Elixir
string = "Elixir é incrível!"
IO.puts length(string)
```

O código acima irá imprimir `17`, que é o tamanho da string `Elixir é incrível!`. Você também pode usar a função `String.length/1` para obter o mesmo resultado. Entretanto, vale ressaltar que essa função só funcionará para strings codificadas em UTF-8.

## Mergulho Profundo

Agora, vamos entender um pouco mais sobre como a função `length/1` funciona. Essa função aceita um argumento, que pode ser qualquer tipo de dado. Ao receber uma string como argumento, a função irá percorrer cada caractere da string e contá-los. Isso inclui espaços, pontuações e qualquer outro caractere. Em essência, essa função está contando o número de elementos em uma lista de caracteres.

Existem também outras formas de encontrar o comprimento de uma string em Elixir, como utilizar a função `Enum.count/1` e a propriedade `String.length/1`. Cada uma dessas opções tem suas próprias particularidades e é importante entender como elas funcionam para escolher qual se adequa melhor à sua necessidade.

## Veja também

- [Documentação Elixir - length/1](https://hexdocs.pm/elixir/Kernel.html#length/1)
- [Documentação Elixir - String.length/1](https://hexdocs.pm/elixir/String.html#length/1)
- [Documentação Elixir - Enum.count/1](https://hexdocs.pm/elixir/Enum.html#count/1)