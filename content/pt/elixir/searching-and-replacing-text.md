---
title:                "Pesquisando e substituindo texto"
date:                  2024-01-20T17:57:29.021612-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Buscar e substituir texto é o processo de localizar sequências específicas num texto e substituí-las por outras. Programadores fazem isso constantemente para corrigir erros, atualizar códigos ou modificar dados rapidamente sem ter que refazer tudo manualmente.

## Como Fazer:

```elixir
# Para substituir todas as ocorrências de uma string:
original = "Elixir é incrível e fácil de aprender. Elixir agiliza o desenvolvimento."
modified = String.replace(original, "Elixir", "Erlang")

IO.puts modified
# Saída: Erlang é incrível e fácil de aprender. Erlang agiliza o desenvolvimento.

# Para substituir a primeira ocorrência de uma string:
modified_once = String.replace(original, "Elixir", "Erlang", global: false)

IO.puts modified_once
# Saída: Erlang é incrível e fácil de aprender. Elixir agiliza o desenvolvimento.

# Usando expressões regulares:
regex_modified = String.replace(original, ~r/Elixir/, "Erlang")

IO.puts regex_modified
# Saída: Erlang é incrível e fácil de aprender. Erlang agiliza o desenvolvimento.
```

## Aprofundando

A função de busca e substituição não é exclusiva do Elixir; ela existe em diversas linguagens de programação e editores de texto há anos. Nas origens da edição de texto computacional, utilitários como `sed` no UNIX já permitiam essas operações através da linha de comando. No Elixir, busca e substituição é frequentemente realizada com as funções `String.replace/3` e `String.replace/4`, que são parte do módulo `String`, ou com expressões regulares usando a sigil `~r/`. A primeira opção é boa para substituições diretas, enquanto a segunda é mais poderosa e flexível quando padrões complexos estão envolvidos. Na prática, ao escolher entre strings normais e expressões regulares, pondera-se a simplicidade versus a necessidade de um padrão mais complexo de busca.

## Veja Também

- [Documentação oficial da função String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [Erlang's regex library documentation](https://erlang.org/doc/man/re.html)
- [Learn Regular Expressions (Regex)](https://www.regular-expressions.info/tutorial.html)
- [Guia rápido de Elixir](https://elixir-lang.org/getting-started/introduction.html)
