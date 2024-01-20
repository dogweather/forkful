---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

---

## O Que e Porquê?

Comparar duas datas é verificar a diferença entre elas. Isso é importante para os programadores pois permite avaliar o tempo decorrido entre eventos, ajudando na gestão de tarefas, lembretes, temporizadores, etc.

## Como fazê-lo:

Aqui está um exemplo de comparação de duas datas no Elixir:

```Elixir
defmodule Comparacao do
  def compare(datas) do
    Data.diff(datas.data1, datas.data2, :second)
  end
end

datas = %{data1: ~U[2019-01-01T00:00:00Z], data2: ~U[2019-01-02T00:00:00Z]}
IO.inspect(Comparacao.compare(datas))
```

A saída será:

```Elixir
86400
```

O resultado é a diferença em segundos entre as duas datas.

## Deep Dive:

A função `diff/3` foi introduzida no Elixir 1.8.0 e faz parte do módulo de "Data". Permite calcular a diferença entre duas datas com base em várias unidades (segundos, minutos, dias, etc.). Antes disso, os programadores tinham que calcular manualmente subtraindo os diferentes componentes da data (ano, mês, dia, etc.).

Existem outras formas de comparar datas - uma abordagem comum é converter ambos para um formato comum, como um 'timestamp', e depois comparar esses valores. No entanto, a função `diff/3` é uma maneira conveniente e legível de fazer isso.

Esse recurso permite uma comparação precisa mesmo através dos fusos horários. A função `diff/3` lida implicitamente com as diferenças de fuso horário quando compara as datas, além de levar em consideração coisas como mudanças de horário de verão.

## Veja Também:

* [Documentação oficial do Elixir](https://elixir-lang.org/docs.html): uma excelente fonte para ler mais sobre a linguagem de programação Elixir e suas funcionalidades.