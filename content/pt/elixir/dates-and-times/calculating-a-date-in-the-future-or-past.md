---
date: 2024-01-20 17:30:54.497848-07:00
description: "Como Fazer: Calcular datas no passado e no futuro n\xE3o \xE9 um conceito\
  \ novo. Desde que os calend\xE1rios foram criados, as pessoas querem saber datas\
  \ futuras ou\u2026"
lastmod: '2024-04-05T21:53:46.579116-06:00'
model: gpt-4-1106-preview
summary: "Calcular datas no passado e no futuro n\xE3o \xE9 um conceito novo."
title: Calculando uma data no futuro ou passado
weight: 26
---

## Como Fazer:
```elixir
# Adicionando dias a uma data
data_hoje = Date.utc_today()
# => ~D[2023-04-05]

data_futura = Date.add(data_hoje, 10)
# => ~D[2023-04-15]

# Subtraindo dias de uma data
data_passada = Date.add(data_hoje, -5)
# => ~D[2023-03-31]

# Trabalhando com meses e anos usando a biblioteca Timex
{:ok, data} = Date.new(2023, 4, 5)
# => {:ok, ~D[2023-04-05]}

data_2_meses_frente = Timex.add(data, Timex.Duration.from_months(2))
# => ~D[2023-06-05]

data_1_ano_atras = Timex.shift(data, years: -1)
# => ~D[2022-04-05]
```

## Aprofundamento:
Calcular datas no passado e no futuro não é um conceito novo. Desde que os calendários foram criados, as pessoas querem saber datas futuras ou recordar as passadas. Em computação, essa necessidade é ainda mais crítica para funções como cronogramas de pagamento, lembretes, ou funções de tempo real. 

No Elixir, a funcionalidade básica de trabalhar com datas vem com o módulo `Date`, que faz parte da biblioteca padrão. Para funcionalidades mais avançadas, muitos programadores utilizam a biblioteca Timex, que fornece uma rica API para manipulação de datas e tempos. Timex facilita o trabalho com fusos horários e períodos mais complexos, como meses e anos, que podem ter quantidades variáveis de dias.

Outras alternativas incluem a biblioteca Calendar, que oferece suporte a diferentes calendários, e até mesmo funções escritas manualmente para casos muito específicos ou simples. A escolha de ferramenta depende das necessidades do projeto.

## Veja Também:
- [Documentação oficial do módulo Date](https://hexdocs.pm/elixir/Date.html)
- [GitHub da biblioteca Timex](https://github.com/bitwalker/timex)
- [Documentação oficial do Elixir](https://elixir-lang.org/docs.html)
