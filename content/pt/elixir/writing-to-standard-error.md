---
title:                "Elixir: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no erro padrão?

Escrever no erro padrão (standard error) é uma habilidade importante para qualquer programador de Elixir. Ela permite que você imprima informações de erro detalhadas durante a execução do seu código, o que pode ser muito útil para depurar e corrigir problemas em suas aplicações.

## Como fazer

Para escrever no erro padrão em Elixir, basta utilizar a função `IO.puts/2`, passando como primeiro argumento o atom `:stderr` para indicar que a mensagem deve ser escrita nesse canal de saída. Veja um exemplo:

```Elixir
IO.puts(:stderr, "Oops, algo deu errado!")
```

Isso irá imprimir a mensagem no console da sua aplicação, acompanhada da informação de qual módulo e linha de código ela foi chamada. Por exemplo:

```bash
Oops, algo deu errado!
** (RuntimeError) Erro na linha 5 do módulo MyApp
```

## Profundidade: Explorando a escrita no erro padrão

Quando você escreve no erro padrão, também pode utilizar a função `IO.inspect/2` para imprimir valores de variáveis e expressões. Isso é especialmente útil para entender o comportamento do seu código em diferentes momentos da execução.

Além disso, você pode utilizar o módulo `Logger` para escrever no erro padrão de forma mais sofisticada, permitindo que você personalize o formato e o nível de logs de acordo com suas necessidades.

## Veja também

- [Documentação oficial do Elixir sobre escrever no erro padrão](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Guia completo sobre uso do módulo Logger em Elixir](https://elixir-lang.org/getting-started/logger.html)