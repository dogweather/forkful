---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removendo Caracteres Correspondentes ao Padrão em Elixir

## O que & Por quê?

Remover caracteres correspondentes a um padrão é uma operação de manipulação de string em que caracteres específicos de uma string são removidos com base em um padrão definido. Os programadores frequentemente fazem isso para limpeza e formatação de dados.

## Como fazer:

O Elixir fornece funções incorporadas que podemos usar para essa finalidade. O exemplo a seguir mostra como você pode remover caracteres que correspondem a um padrão específico.

```elixir
IO.puts(String.replace("Olá, Mundo!", ",", "")) # Output: "Olá Mundo!"
```

No exemplo acima, estamos usando a função `String.replace/3` do módulo String para substituir todas as ocorrências da "," na string dada por "" (nada).

## Mergulho Profundo 

Na verdade, essa técnica se origina da necessidade de limpar e formatar dados para uso posterior. Isso é especialmente útil em trabalhos como análise de dados, onde os dados precisam estar no formato correto para obter resultados precisos.

Uma alternativa é usar expressões regulares para corresponder a um padrão, embora isso possa ser um pouco mais complexo dependendo do padrão.

Quanto à implementação, a função String.replace/3 faz uso do módulo `:binary` do Erlang, um detalhe interessante sobre a interoperabilidade entre Elixir e Erlang.

## Veja também

Para continuar aprendendo sobre manipulação de string em Elixir, confira os seguintes recursos:

1. [Guia oficial de String em Elixir](https://hexdocs.pm/elixir/String.html)
2. [Expressões regulares em Elixir](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
3. [Documentação do módulo :binary do Erlang](http://erlang.org/doc/man/binary.html)