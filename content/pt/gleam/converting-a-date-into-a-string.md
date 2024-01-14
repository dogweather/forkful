---
title:                "Gleam: Convertendo uma data em uma string"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Conversão de tipos de dados é uma tarefa comum na programação. Converter uma data em uma string é útil quando você precisa exibir a data de uma maneira legível para o usuário ou armazenar a data em um formato específico.

## Como fazer

A linguagem de programação Gleam oferece uma maneira fácil de converter uma data em uma string. Vamos ver um exemplo de código que pega a data atual e a converte em uma string no formato "dd/mm/aaaa":

```
Gleam
import Time.DateTime

let data_atual = DateTime.now()
let data_string = DateTime.to_string(data_atual, "%d/%m/%Y")
```

Aqui, importamos o módulo "Time.DateTime", que nos permite trabalhar com datas. Em seguida, usamos a função "now()" para obter a data e hora atuais. Em seguida, usamos a função "to_string()" para converter a data em uma string no formato desejado.

Aqui está o resultado do código acima:

```
"01/01/2021"
```

Você também pode especificar uma data específica para converter em uma string. Por exemplo, se quisermos converter a data de 25 de dezembro de 2020 em uma string no formato "mm/dd/yy", podemos fazer o seguinte:

```
Gleam
import Time.DateTime

let data_especifica = DateTime.from_iso8601("2020-12-25")
let data_string = DateTime.to_string(data_especifica, "%m/%d/%y")
```

E o resultado seria: "12/25/20".

## Profundidade do assunto

Na linguagem Gleam, o formato da string de data é especificado usando códigos de formatação, como "%d" para o dia, "%m" para o mês ou "%Y" para o ano. Existem muitos códigos de formatação diferentes disponíveis para você escolher, e você também pode inserir texto adicional na string para personalizá-la ainda mais.

Além disso, o módulo "Time.DateTime" oferece outras funções úteis para manipular datas, como "add_seconds()" para adicionar segundos a uma data, "subtract_days()" para subtrair dias de uma data e "to_utc()" para converter uma data em UTC.

## Veja também

- Documentação oficial Gleam sobre manipulação de datas e horas: https://gleam.run/libraries/time
- Tutorial de Gleam sobre manipulação de datas e horas: https://dev.to/ggburr/tutorial-time-in-gleam-36ml