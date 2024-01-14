---
title:                "Gleam: Calculando uma data no futuro ou passado."
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser útil em várias situações, como agendar eventos ou tarefas, planejar viagens ou acompanhar prazos importantes.

## Como fazer

Para realizar essa tarefa no Gleam, podemos utilizar a função `DateTime.add` ou `DateTime.sub`. A primeira recebe uma data e um intervalo de tempo como parâmetros e retorna uma nova data avançando o intervalo de tempo especificado. Já a segunda faz o oposto, retrocedendo o intervalo de tempo fornecido.

Vejamos um exemplo de uso da função `DateTime.add` para calcular uma data 1 semana no futuro:

```Gleam
import gleam/datetime.{ DateTime, Week }

let data_atual = DateTime.now()
let data_futura = DateTime.add(Week(1), data_atual)
```

Neste caso, a variável `data_futura` terá o valor correspondente a uma semana após a data atual.

## Mergulho profundo

Caso queiramos calcular uma data futura ou passada com base em um intervalo de tempo específico, podemos utilizar a função `DateTime.from_parts`. Esta função permite fornecer diretamente os valores para ano, mês, dia, hora, minuto e segundo da data desejada. Por exemplo, para calcular uma data daqui a 2 anos e 3 meses, podemos fazer o seguinte:

```Gleam
import gleam/datetime.{ DateTime }

let data_futura = DateTime.from_parts(year=DateTime.now().year + 2, month=DateTime.now().month + 3, day=DateTime.now().day, hour=DateTime.now().hour, minute=DateTime.now().minute, second=DateTime.now().second)
```

Note que utilizamos os valores atuais para hora, minuto e segundo, já que não foi especificado um intervalo de tempo para estes.

## Veja também

- [Documentação oficial do módulo DateTime do Gleam](https://gleam.run/modules/gleam_datetime/latest/DateTime.html)
- [Tutorial de programação em Gleam](https://gleam.run/blog/getting-started-with-gleam-to-build-a-todo-backend.html)
- [Exemplos de projetos em Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)