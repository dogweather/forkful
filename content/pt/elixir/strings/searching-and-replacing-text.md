---
date: 2024-01-20 17:57:29.021612-07:00
description: "Como Fazer: A fun\xE7\xE3o de busca e substitui\xE7\xE3o n\xE3o \xE9\
  \ exclusiva do Elixir; ela existe em diversas linguagens de programa\xE7\xE3o e\
  \ editores de texto h\xE1 anos. Nas\u2026"
lastmod: '2024-04-05T21:53:46.550023-06:00'
model: gpt-4-1106-preview
summary: "A fun\xE7\xE3o de busca e substitui\xE7\xE3o n\xE3o \xE9 exclusiva do Elixir;\
  \ ela existe em diversas linguagens de programa\xE7\xE3o e editores de texto h\xE1\
  \ anos."
title: Pesquisando e substituindo texto
weight: 10
---

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
