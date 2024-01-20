---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Elixir: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que e por que?

Calcular uma data no futuro ou no passado é simplesmente uma maneira de adicionar ou subtrair um certo período de tempo de uma data específica. Os programadores fazem isso para manipular datas para várias finalidades - monitoramento do tempo, agendamento, etc.

## Como Faz:

Aqui está um exemplo simples de como calcular datas futuras ou passadas em Elixir.

- Adicionando dias a uma data

```elixir
date = Date.new(2021, 12, 1)
new_date = Date.add(date, 7)
IO.puts(new_date)

# Saída:
# 2021-12-08
```

- Subtraindo dias de uma data

```elixir
date = Date.new(2021, 12, 1)
new_date = Date.add(date, -7)
IO.puts(new_date)

# Saída:
# 2021-11-24
```

## Aprofundando:

- **Contexto histórico:** 
A capacidade de calcular datas no futuro ou no passado tem sido uma parte essencial de muitos sistemas desde os tempos antigos. Ela é usada em tudo, desde o agendamento de tarefas até a formulação de previsões baseadas no tempo.

- **Alternativas:**
Existem várias outras bibliotecas e pacotes que você pode usar para manipular datas em Elixir, como a biblioteca 'Timex'.

```elixir
{:ok, date} = Date.new(2021, 12, 01)
Timex.shift(date, days: 10)

# Saída:
# #Date<2021-12-11>
```

- **Detalhes de implementação:**
Em Elixir, a data é lidada como uma estrutura, o que significa que é imutável. Adicionar ou subtrair dias cria uma nova estrutura de data em vez de modificar a original.

## Veja também:

Se você quer estudar mais sobre o trabalho com datas no Elixir, aqui estão alguns recursos-chave:

- Documentação oficial do Elixir para módulo 'Date': https://hexdocs.pm/elixir/Date.html
- Timex, uma biblioteca rica de funções relacionadas ao tempo no Elixir: https://hexdocs.pm/timex/readme.html
- Curso de Elixir na Pluralsight: https://www.pluralsight.com/courses/elixir-getting-started