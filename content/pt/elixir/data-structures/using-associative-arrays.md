---
aliases:
- /pt/elixir/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:49.538142-07:00
description: "Em Elixir, os arrays associativos, chamados de Mapas, s\xE3o cole\xE7\
  \xF5es de pares chave-valor onde uma chave \xFAnica aponta para um valor. Eles s\xE3\
  o extremamente\u2026"
lastmod: 2024-02-18 23:08:57.837186
model: gpt-4-0125-preview
summary: "Em Elixir, os arrays associativos, chamados de Mapas, s\xE3o cole\xE7\xF5\
  es de pares chave-valor onde uma chave \xFAnica aponta para um valor. Eles s\xE3\
  o extremamente\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O que & Por quê?

Em Elixir, os arrays associativos, chamados de Mapas, são coleções de pares chave-valor onde uma chave única aponta para um valor. Eles são extremamente úteis para armazenar e recuperar dados de forma rápida, tornando seu código mais limpo e sua vida mais fácil.

## Como fazer:

Criar um Mapa é simples. Você usa a sintaxe `%{}`, assim:

```elixir
meu_mapa = %{"nome" => "Alex", "idade" => 32}
IO.inspect(meu_mapa)
```

Acessar valores é feito usando as chaves:

```elixir
IO.puts meu_mapa["nome"]
```
Saída: `Alex`

Para adicionar ou atualizar valores, você pode usar a função `Map.put/3`:

```elixir
mapa_atualizado = Map.put(meu_mapa, "localização", "NY")
IO.inspect(mapa_atualizado)
```
Saída: `%{"idade" => 32, "localização" => "NY", "nome" => "Alex"}`

Remover chaves é igualmente simples com `Map.delete/2`:

```elixir
mapa_reduzido = Map.delete(mapa_atualizado, "idade")
IO.inspect(mapa_reduzido)
```
Saída: `%{"localização" => "NY", "nome" => "Alex"}`

## Aprofundando

Mapas em Elixir são uma evolução dos antigos tipos de armazenamento chave-valor, como Hashes em Ruby ou Dicionários em Python. Eles permitem buscas e inserções mais eficientes, tornando-os uma opção preferencial para programação moderna em Elixir. Vale notar que, antes dos Mapas, Elixir utilizava os módulos HashDict e Dict, que estão depreciados agora.

No entanto, para cenários que requerem dados ordenados, você pode considerar listas de palavras-chave em Elixir. Essas são listas de tuplas, eficientes para coleções menores mas não tão amigáveis em termos de desempenho para grandes conjuntos de dados como os Mapas.

Tenha em mente que os Mapas armazenam suas chaves em uma estrutura "plana", tornando o acesso direto a valores aninhados um pouco complicado. Para aninhamento profundo, você pode considerar o acesso estruturado através das funções `get_in`, `put_in`, `update_in`, e `get_and_update_in`, que permitem uma abordagem mais dinâmica para a manipulação de dados aninhados.

Em resumo, enquanto Mapas são sua opção de partida para necessidades de arrays associativos em Elixir, a linguagem oferece uma rica variedade de estruturas de dados para cada cenário, incentivando você a escolher a ferramenta certa para o trabalho.
