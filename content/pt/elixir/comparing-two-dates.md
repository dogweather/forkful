---
title:                "Elixir: Comparando duas datas"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Porque

Comparar datas é uma tarefa comum na programação, especialmente quando se trata de dados temporais. Ao comparar duas datas, podemos determinar a ordem cronológica e realizar diferentes ações com base nesta informação. No Elixir, existem várias maneiras de comparar datas, cada uma com suas próprias vantagens e desvantagens. Neste artigo, vamos explorar como comparar datas usando Elixir.

##Como Fazer

Podemos comparar datas no Elixir usando os operadores de comparação padrão, como `>`, `<` e `==`. No entanto, o Elixir também oferece uma função específica chamada `Date.compare/2` que nos permite comparar duas datas de forma mais precisa. Vamos dar uma olhada em como usá-la:

```Elixir
iex> Date.compare(~D[2020-01-01], ~D[2019-01-01])
1
```

Neste exemplo, usamos a função `Date.compare/2` para comparar as datas 1 de janeiro de 2020 e 1 de janeiro de 2019. A função retorna o número 1, indicando que a primeira data é posterior à segunda. Se a data fosse anterior, o número seria -1 e se fossem iguais, seria 0.

Também podemos usar a função `Date.diff/2` para obter a diferença entre duas datas em dias, semanas, meses ou anos:

```Elixir
iex> Date.diff(~D[2020-01-01], ~D[2019-01-01], :days)
365
```

Este é apenas um exemplo básico de como comparar datas no Elixir. Você pode aprender mais sobre essas funções e suas opções de parâmetros na documentação oficial do Elixir.

##Mergulho Profundo

Ao comparar datas, devemos ter cuidado com questões de fuso horário e horário de verão. Se você estiver trabalhando com datas em diferentes fusos horários, é importante converter as datas para um único fuso horário antes de compará-las. Isso pode ser feito usando a função `DateTime.shift_zone/3`.

Além disso, algumas linguagens de programação têm bibliotecas específicas para lidar com datas e horários, como o MomentJS para JavaScript. No entanto, o Elixir tem um módulo padrão chamado `Calendar` que oferece uma ampla gama de funções para manipular e comparar datas e horários. Certifique-se de explorar todas as opções disponíveis e escolher a que melhor se adapta às suas necessidades.

##Veja Também

- [Documentação do Elixir sobre manipulação de datas](https://hexdocs.pm/elixir/Calendar.html)
- [Artigo sobre como manipular datas no Elixir](https://dev.to/elixirlabs/playing-with-dates-and-times-in-elixir-3bef)
- [GitHub repositório com exemplos de código de comparação de datas em Elixir](https://github.com/filipebarros/comparing-dates-elixir)