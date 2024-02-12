---
title:                "Organizando o código em funções"
aliases:
- pt/elixir/organizing-code-into-functions.md
date:                  2024-01-26T01:09:51.725097-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Organizar o código em funções significa agrupar operações relacionadas em blocos reutilizáveis. Fazemos isso para melhorar a legibilidade e manutenibilidade, reduzir a duplicação de código e simplificar os testes.

## Como fazer:
Vamos criar uma função simples em Elixir para capitalizar palavras:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
Saída:
```
Hello Elixir World
```
Aqui, empacotamos de forma organizada a lógica de capitalização de palavras numa função chamada `capitalize_words`.

## Aprofundamento
No Elixir, e no ecossistema mais amplo da Erlang VM, as funções são cidadãos de primeira classe, herdando a filosofia de decompor problemas em partes menores, gerenciáveis e isoladas. Historicamente, esta abordagem funcional tem raízes no cálculo lambda e nos Lisps, promovendo a filosofia de código como dados.

Alternativas para organizar o código podem incluir o uso de macros ou processos em Elixir para tarefas repetitivas ou concorrentes, respectivamente. Em termos de implementação, as funções do Elixir podem lidar com correspondência de padrões e receber diferentes argumentos (aridade), concedendo-lhes versatilidade.

## Veja Também
- [Documentação oficial do Elixir sobre funções](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas' "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
