---
date: 2024-01-26 03:48:23.152351-07:00
description: "Usar um depurador em Elixir envolve percorrer seu c\xF3digo, inspecionar\
  \ vari\xE1veis e rastrear fluxos para eliminar bugs. Os programadores fazem isso\
  \ para\u2026"
lastmod: 2024-02-19 22:05:05.318477
model: gpt-4-0125-preview
summary: "Usar um depurador em Elixir envolve percorrer seu c\xF3digo, inspecionar\
  \ vari\xE1veis e rastrear fluxos para eliminar bugs. Os programadores fazem isso\
  \ para\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Por Quê?
Usar um depurador em Elixir envolve percorrer seu código, inspecionar variáveis e rastrear fluxos para eliminar bugs. Os programadores fazem isso para entender o inesperado e garantir que suas aplicações se comportem conforme o projetado.

## Como usar:
O Elixir vem com um depurador gráfico embutido chamado `:debugger`. Para usá-lo, você precisará iniciá-lo e anexá-lo ao seu processo em execução.

Primeiro, garanta que você tenha `:debugger` iniciado dentro de uma sessão `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Agora, interprete o módulo de código que você deseja depurar:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Você pode definir um ponto de interrupção:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

E então, execute sua função para alcançar o ponto de interrupção e percorrer seu código:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# O depurador pausará a execução na linha com o ponto de interrupção
```

## Aprofundando
Antes do `:debugger` de Elixir, o Erlang forneceu o depurador que o Elixir usa; ele é robusto e excelente no manejo de processos concorrentes, um ponto forte da VM do Erlang (BEAM). Diferente de alguns outros depuradores, o `:debugger` não permite a modificação de variáveis on the fly, devido à natureza imutável dos dados em Elixir. Quanto às alternativas, você tem o `IEx.pry` que permite pausar a execução e pular para um REPL em qualquer ponto do seu código, o que pode ser extremamente útil.

Enquanto o `:debugger` é bom para uma interface gráfica, alguns podem preferir a ferramenta embutida `:observer` que também oferece inspeção de processos e métricas do sistema, embora não seja especificamente direcionada para percorrer o código. A comunidade do Elixir também contribui com ferramentas como `visualixir` e `rexbug`, expandindo o ecossistema de ferramentas de depuração além dos padrões.

## Veja Também
- Guia Oficial de Início Rápido do Elixir sobre Depuração: https://elixir-lang.org/getting-started/debugging.html
- Documentação do `:debugger` do Erlang: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Discussões no Fórum de Elixir sobre Técnicas de Depuração: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
