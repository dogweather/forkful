---
title:                "Elixir: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

A manipulação de strings é uma tarefa comum em qualquer linguagem de programação, e Elixir não é exceção. Encontrar o comprimento de uma string pode parecer simples, mas há várias nuances a serem consideradas.

## Como fazer

Para encontrar o comprimento de uma string em Elixir, podemos usar a função `String.length/1`. Veja um exemplo abaixo:

```Elixir
iex> String.length("Olá, mundo!")
12
```

Como você pode ver, a função retornará o número de caracteres na string, incluindo espaços e pontuação.

Podemos também usar a função `String.codepoints/1` para obter uma lista de cada caracter na string e, em seguida, usar a função `Enum.count/1` para contar os elementos da lista. Confira o exemplo abaixo:

```Elixir
iex> "Hello" |> String.codepoints() |> Enum.count()
5
```

Esta abordagem é útil se quisermos lidar com caracteres multibyte, já que a função `String.length/1` conta o número de bytes e não de caracteres.

## Mergulho profundo

Embora a função `String.length/1` possa parecer direta, é importante entender como ela lida com caracteres multibyte e como isso pode afetar o nosso código.

Por exemplo, se usarmos a função em uma string contendo caracteres multibyte, como "ámago", o resultado pode ser surpreendente:

```Elixir
iex> String.length("ámago")
6
```

Isso acontece porque no Elixir, as strings são representadas internamente como listas de bytes. Então, quando usamos a função `String.length/1`, ela simplesmente conta o número de elementos na lista. Isso pode ser um problema se estivermos lidando com caracteres que ocupam mais de um byte, pois o comprimento retornado não corresponderá ao número real de caracteres na string.

Uma maneira de contornar isso é usando a função `String.graphemes/1`, que divide a string em suas unidades de caracteres gráficos. Confira o exemplo abaixo:

```Elixir
iex> "ámago" |> String.graphemes() |> Enum.count()
5
```

Agora, o resultado corresponde corretamente ao número de caracteres na string.

## Veja também

- [Documentação oficial do Elixir](https://elixir-lang.org/docs.html)
- [Artigo sobre manipulação de strings em Elixir](https://medium.com/@squareroots-mp/strings-manipulation-in-elixir-327f3b7c7a18)
- [Vídeo tutorial em português sobre Elixir](https://www.youtube.com/watch?v=G8RrJU_GvRg)