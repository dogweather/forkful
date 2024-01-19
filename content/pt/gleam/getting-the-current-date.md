---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? ["O Que e Porquê?"]

Capturar a data atual em programação permite rastrear eventos em tempo real. Esta ação é essencial para registros, log de eventos ou até para agendar tarefas.

## How to:  ["Como Fazer:"]

No Gleam, você pode obter a data atual usando a biblioteca `gleam/calendar`. Veja um exemplo simples:

```Gleam
import gleam/calendar.{now, to_erlang}

fn main() {
  let {calendar.year, calendar.month, calendar.day} = now()
  |> to_erlang()
  let _: String = io.println("A data atual é: " ++ year.to_string() ++ "-" ++ month.to_string() ++ "-" ++ day.to_string())
}
```

Ao ser executado, obtenha uma saída no format "A data atual é: AAAA-MM-DD".

## Deep Dive ["Aprofundando"]

Historicamente, muitas linguagens de programação manipulam datas e horas de maneira semelhante. No entanto, as diferenças surgem quando consideramos a região do usuário, fusos horários, horário de verão, entre outros.

Alternativas para a obtenção da data atual incluem o uso de APIs ou bibliotecas externas. No Gleam, a utilização da biblioteca `gleam/calendar` é a escolha de muitos por sua simplicidade e eficiência.

A função `now` do `gleam/calendar` busca a data e a hora atual do sistema. Para extrair somente a data, convertemos o resultado para format Erlang e, em seguida, selecionamos ano, mês e dia.

## See Also ["Veja Também"]

- Documentação oficial de Gleam: https://gleam.run/documentation/
- Biblioteca `gleam/calendar`: https://hexdocs.pm/gleam_stdlib/gleam/calendar.html

Espero que este artigo tenha sido útil para você entender como obter a data atual no Gleam. Feliz programação!