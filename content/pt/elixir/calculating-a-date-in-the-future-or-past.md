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

## O que e Por Que?

Calcular uma data no futuro ou no passado é o processo de determinar uma data especifica com base em uma data de referência e um intervalo de tempo. Os programadores realizam esse processo para automatizar tarefas que envolvem datas, como agendar eventos ou calcular a idade de uma pessoa.

## Como Fazer:

Um exemplo simples de como calcular uma data no futuro usando Elixir é usando a função ```DateTime.add/3```. Vamos supor que queremos saber a data daqui a 10 dias a partir de hoje. Podemos escrever o seguinte código:

```elixir
DateTime.add(DateTime.utc_now(), 10, :day)
```

O resultado seria uma data no futuro, 10 dias a partir de hoje, no formato de data e hora UTC. Também podemos calcular uma data no passado usando a função ```DateTime.add/3```, usando um número negativo como o segundo argumento. Por exemplo, para saber a data de 10 dias atrás, podemos escrever o seguinte código:

```elixir
DateTime.add(DateTime.utc_now(), -10, :day)
```

O resultado seria uma data no passado, 10 dias antes da data atual, no formato de data e hora UTC.

## Imersão Profunda:

Calcular datas no futuro ou no passado tem sido uma tarefa importante para os programadores desde o início da computação. Antes da existência de linguagens de programação, as pessoas precisavam fazer esses cálculos manualmente, o que era um processo demorado e propenso a erros. Com o avanço da tecnologia e o desenvolvimento de linguagens de programação, calcular datas no futuro ou no passado se tornou muito mais fácil e preciso.

Além do Elixir, existem várias outras linguagens de programação que podem ser usadas para calcular datas no futuro ou no passado, como Python, JavaScript e Ruby. No entanto, o Elixir tem uma sintaxe simples e elegante para manipulação de datas e tem o benefício adicional de ser executado na máquina virtual de Erlang, tornando-o extremamente rápido e com alta capacidade de escalabilidade.

O processo de calcular uma data no futuro ou no passado no Elixir envolve o uso de uma série de funções integradas, como ```DateTime.utc_now/0``` para obter a data atual em UTC e ```DateTime.add/3``` para adicionar ou subtrair um intervalo de tempo de uma data. É importante sempre verificar a documentação de cada função para garantir que o código esteja correto e que a data calculada seja a desejada.

## Veja Também:

- Documentação oficial do Elixir sobre manipulação de datas: https://hexdocs.pm/elixir/DateTime.html
- Perguntas frequentes sobre manipulação de datas no Elixir: https://stackoverflow.com/questions/tagged/elixir+datetime
- Tutorial em vídeo sobre como usar o Elixir para calcular datas futuras e passadas: https://www.youtube.com/watch?v=lzu1Q1WJ_O8