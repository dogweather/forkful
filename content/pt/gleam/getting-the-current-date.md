---
title:                "Obtendo a data atual"
html_title:           "Gleam: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Obter a data atual é uma tarefa comum em muitos programas. Pode ser útil para registrar quando um evento ocorreu, exibir a data para o usuário ou até mesmo para realizar cálculos de tempo. Felizmente, com Gleam, é muito fácil obter a data atual.

## Como obter a data atual

Para obter a data atual em Gleam, podemos usar a função `Time.now()`, que retorna a data e a hora atuais em um formato legível. Veja um exemplo abaixo:

```Gleam
import gleam/time

let data_atual = Time.now()
```

Ao imprimir a `data_atual`, veremos algo como `2021-07-15T13:41:25.135076Z`.

Também podemos acessar partes específicas da data, como o dia, mês e ano. Podemos fazer isso usando a função `Time.date()` e especificando qual parte da data queremos, como no exemplo abaixo:

```Gleam
import gleam/time

let data_atual = Time.date(Time.Year)
```

Isso retornará apenas o ano atual, que seria `2021`.

## Aprofundando-se

A obtenção da data atual pode não parecer tão complexa, mas na realidade, pode ser difícil de implementar de forma confiável. Isso ocorre porque a data e a hora são influenciadas pela configuração do sistema e pelo fuso horário, que podem mudar com os servidores e as localizações geográficas. É importante levar isso em consideração ao utilizar a função `Time.now()` em seus programas Gleam.

## Veja também

- Documentação da função `Time.now()`: https://gleam.run/modules/time#now
- Tutorial sobre como trabalhar com datas em Gleam: https://dev.to/mtnuckle/working-with-dates-in-gleam-124a