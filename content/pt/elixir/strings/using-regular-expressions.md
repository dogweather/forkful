---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:33.965900-07:00
description: "Express\xF5es regulares (regex) em Elixir s\xE3o usadas para buscar,\
  \ combinar e manipular strings baseadas em padr\xF5es espec\xEDficos. Programadores\
  \ utilizam regex\u2026"
lastmod: 2024-02-19 22:05:05.303169
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) em Elixir s\xE3o usadas para buscar, combinar\
  \ e manipular strings baseadas em padr\xF5es espec\xEDficos. Programadores utilizam\
  \ regex\u2026"
title: "Usando express\xF5es regulares"
---

{{< edit_this_page >}}

## O que & Por quê?

Expressões regulares (regex) em Elixir são usadas para buscar, combinar e manipular strings baseadas em padrões específicos. Programadores utilizam regex para tarefas como validar formatos (email, URLs), analisar logs, ou extrair dados, graças à sua eficiência e versatilidade no manuseio de strings.

## Como fazer:

Elixir utiliza o módulo `Regex`, aproveitando a biblioteca regex do Erlang, para operações com regex. Aqui estão usos básicos:

```elixir
# Combinando um padrão - Retorna a primeira combinação
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Saída: ["hello"]

# Encontrando todas as combinações
all_matches = Regex.scan(~r/\d/, "Há 2 maçãs e 5 laranjas.")
IO.inspect(all_matches) # Saída: [["2"], ["5"]]

# Substituindo partes de uma string
replaced_string = Regex.replace(~r/\s+/, "Elixir é divertido", "_")
IO.inspect(replaced_string) # Saída: "Elixir_é_divertido"
```

Para padrões e funcionalidades mais complexas, você pode considerar usar bibliotecas de terceiros, embora para a maioria das tarefas centrais de combinação e manuseio de strings e padrões, o módulo `Regex` incorporado ao Elixir é bastante poderoso.

Para realizar uma combinação que não diferencia maiúsculas de minúsculas, use a opção `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Saída: ["Hello"]
```

Expressões regex podem ser pré-compiladas para eficiência quando usadas múltiplas vezes:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Saída: ["hello"]
```

Elixir também suporta capturas nomeadas, que podem ser muito úteis para extrair partes específicas de uma string, tornando seu código mais legível:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Saída: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Esta breve visão geral destaca a facilidade com que Elixir lida com expressões regulares, possibilitando técnicas poderosas de manipulação de strings e extração de dados.
