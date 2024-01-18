---
title:                "Análise de uma data de uma sequência de caracteres."
html_title:           "Kotlin: Análise de uma data de uma sequência de caracteres."
simple_title:         "Análise de uma data de uma sequência de caracteres."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Ao analisar uma data a partir de uma string, os programadores estão convertendo a informação de uma string em um formato de data reconhecível pelo computador. Isso é importante para que o programa possa manipular e armazenar corretamente as datas fornecidas pelos usuários ou sistemas externos.

## Como fazer:

```Kotlin
// Exemplo 1: Analisando uma data em formato padrão
val data = LocalDate.parse("30/06/2021")
println(data) // output: 2021-06-30

// Exemplo 2: Analisando uma data em um formato específico
val formato = DateTimeFormatter.ofPattern("dd-MMM-yyyy")
val data = LocalDate.parse("30-Jun-2021", formato)
println(data) // output: 2021-06-30
```

## Profundando:

Ao longo do tempo, diferentes formatos de data foram criados, causando a necessidade de um meio para que os computadores possam interpretar esses formatos. Além do método `parse` usado no exemplo acima, também é possível usar a classe `SimpleDateFormat`, mas ela é considerada obsoleta em relação ao `DateTimeFormatter`. Outra alternativa é utilizar bibliotecas de terceiros, como o "Joda-Time", que também fornecem recursos avançados para lidar com datas.

O processo de análise de uma data também envolve a validação e o tratamento de possíveis erros, como formatos incorretos ou datas inválidas. É importante considerar esses casos ao implementar a análise de datas em um programa.

## Veja também:

- Documentação oficial do Kotlin sobre o `DateTimeFormatter`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/
- Mais informações sobre a biblioteca "Joda-Time": https://www.joda.org/joda-time/