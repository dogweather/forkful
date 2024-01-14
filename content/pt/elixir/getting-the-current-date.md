---
title:                "Elixir: Obtendo a data atual"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por Que

A obtenção da data atual é uma tarefa muito comum e útil em muitos projetos de programação. Pode ser necessário para registrar quando um evento ocorreu, para executar uma ação em um determinado dia ou até mesmo para fins de formatação de dados. Independentemente do motivo, saber como obter a data atual é uma habilidade importante para qualquer programador.

## Como Fazer

Existem várias maneiras de obter a data atual em Elixir. Aqui estão dois exemplos usando as funções `DateTime.utc_now/0` e `NaiveDateTime.utc_now/0`.

```
Elixir DateTime.utc_now()
{:ok, ~U[2021-02-05 15:33:46.733391Z]} 
```

```
Elixir NaiveDateTime.utc_now()
~N[2021-02-05 15:35:07.617465]
```

Ambas as funções retornam um formato de data e hora específico do Elixir. A diferença é que `DateTime.utc_now/0` também inclui informações de fuso horário, enquanto `NaiveDateTime.utc_now/0` não.

Você também pode usar a função `Date.utc_today/0` para obter apenas a data atual, sem a hora.

```
Elixir Date.utc_today()
{:ok, ~D[2021-02-05]}
```

## Mergulho Profundo

Para entender melhor como as funções de data e hora funcionam em Elixir, é importante conhecer o módulo `Calendar`, que fornece funções para manipular e formatar datas e horas.

Ao usar `DateTime.utc_now/0` ou `NaiveDateTime.utc_now/0`, você pode passar opções de formato para alterar a maneira como a data e a hora são exibidas. Por exemplo:

```
Elixir DateTime.utc_now(sectos: true)
{:ok, ~U[2021-02-05 15:41:02.941203Z]}
```

A opção `sectos: true` inclui os segundos no formato da data e hora.

Você também pode usar `Calendar` para converter um formato de data e hora específico em outro. Por exemplo, para converter uma data e hora em um formato legível por humanos, você pode usar a função `strftime/2`. Veja um exemplo:

```
Elixir DateTime.utc_now() |> Calendar.strftime("%d/%m/%Y às %H:%M")
{:ok, "05/02/2021 às 15:44"}
```

Isso retorna a data e a hora no formato "dia/mês/ano às horas:minutos".

Há muito mais recursos e opções disponíveis para trabalhar com datas e horas em Elixir. Então, sempre que estiver trabalhando com tais dados, é uma boa ideia consultar a documentação oficial do Elixir para obter mais informações.

## Veja Também

- Documentação do Elixir sobre datas e horas (https://hexdocs.pm/elixir/Calendar.html)
- Guia de referência para formato de data e hora (https://help.interfaceware.com/v6/date-time-fmt)
- Tutorial de Elixir sobre manipulação de datas (https://elixircasts.io/working-with-dates-and-times-in-elixir)