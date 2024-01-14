---
title:    "Elixir: Concatenando strings"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que concatenar strings em Elixir?

Concatenar strings é uma operação fundamental em muitas linguagens de programação, incluindo Elixir. Isso permite unir duas ou mais strings para formar uma nova string, que pode ser usada para criar mensagens, URLs dinâmicas, consultas de banco de dados e muito mais.

## Como fazer a concatenação de strings em Elixir

Em Elixir, podemos usar o operador `<>` para concatenar strings. Vamos ver um exemplo simples de como fazer isso:

```Elixir
nome = "João"
saudacao = "Olá, " <> nome
IO.puts(saudacao)
```

Neste exemplo, criamos uma variável `nome` com o valor "João" e criamos outra variável `saudacao` que une a string "Olá, " com a variável `nome` usando o operador `<>`. Em seguida, usamos a função `IO.puts` para imprimir a saudação completa "Olá, João". O `<>` também pode ser usado para concatenar mais de duas strings.

## Desvendando a concatenação de strings em Elixir

Ao contrário de algumas linguagens de programação, a concatenação de strings em Elixir é eficiente e não cria novas strings a cada concatenação. Em vez disso, ele usa uma estrutura de dados chamada listas de bytes, que é imutável e permite que as strings sejam compartilhadas sem a necessidade de cópias extras. Isso faz com que a concatenação de strings em Elixir seja muito rápida e eficiente.

Outro aspecto importante a ser considerado é que as strings em Elixir são representadas como listas de caracteres, o que significa que podemos usar funções de lista como `List.delete_at` para remover ou substituir caracteres específicos em uma string.

## Veja também

- [Documentação oficial sobre strings em Elixir](https://hexdocs.pm/elixir/String.html)
- [Artigo sobre listas de bytes em Elixir](https://www.brianstorti.com/the-erlang-elixir-unicode-guide/)
- [Tutorial de Elixir para iniciantes](https://elixir-lang.org/getting-started/introduction.html)