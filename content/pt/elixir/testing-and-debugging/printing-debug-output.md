---
date: 2024-01-20 17:52:25.227074-07:00
description: "Como Fazer: Para imprimir valores durante a execu\xE7\xE3o de um programa\
  \ Elixir, voc\xEA pode usar a fun\xE7\xE3o `IO.inspect/2`, que retorna o valor inspecionado,\u2026"
lastmod: '2024-03-13T22:44:46.240737-06:00'
model: gpt-4-1106-preview
summary: "Para imprimir valores durante a execu\xE7\xE3o de um programa Elixir, voc\xEA\
  \ pode usar a fun\xE7\xE3o `IO.inspect/2`, que retorna o valor inspecionado, permitindo\
  \ que a inspe\xE7\xE3o n\xE3o interrompa o fluxo do seu c\xF3digo."
title: "Exibindo sa\xEDdas de depura\xE7\xE3o"
weight: 33
---

## Como Fazer:
Para imprimir valores durante a execução de um programa Elixir, você pode usar a função `IO.inspect/2`, que retorna o valor inspecionado, permitindo que a inspeção não interrompa o fluxo do seu código.

```elixir
defmodule ExemploDebug do
  def somar_e_inspect(a, b) do
    a + b
    |> IO.inspect(label: "resultado da soma")
  end
end

ExemploDebug.somar_e_inspect(2, 3)
```
Saída esperada:
```
resultado da soma: 5
```
Você pode também usar o `IO.puts/1`, se só quiser imprimir algo simples, sem retornar o valor.

```elixir
IO.puts("Hello, debug!")
```
Saída esperada:
```
Hello, debug!
```

## Aprofundando
Historicamente, a impressão para depuração é um dos métodos de diagnóstico mais antigos e universais. Quase todas as linguagens de programação têm alguma forma de imprimir saídas para ajudar no desenvolvimento e no debug.

No Elixir, além das funções `IO.inspect` e `IO.puts`, existem abordagens mais avançadas como `:debugger`, que inicia um ambiente de depuração completo, e o `:observer`, que permite inspecionar um sistema em execução visualmente.

As macros `Logger.debug/2`, `Logger.info/2`, `Logger.warn/2`, e `Logger.error/2` são alternativas quando se trata de emitir mensagens que podem ser filtradas por níveis de importância e configuradas para serem exibidas ou não, dependendo do ambiente (teste, desenvolvimento, produção).

Detalhes de implementação, como evitar a impressão em produção e a sobrecarga da saída de logs numa aplicação em alta escala, também são assuntos importantes a se considerar.

## Veja Também
- [Documentação oficial de IO](https://hexdocs.pm/elixir/IO.html)
- [Guia sobre Logger](https://hexdocs.pm/logger/Logger.html)
- [Iniciando com o `:debugger` em Elixir](http://elixir-lang.github.io/getting-started/debugging.html#debugger)
