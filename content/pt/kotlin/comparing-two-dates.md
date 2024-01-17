---
title:                "Comparando duas datas"
html_title:           "Kotlin: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Comparar duas datas é uma tarefa comum na programação, que envolve verificar se uma data é igual, anterior ou posterior a outra. Isso é importante para garantir a precisão dos dados e facilitar a tomada de decisões baseadas em datas.

## Como fazer:

Para comparar duas datas em Kotlin, podemos usar a função `compareTo()` da classe `LocalDate`. Por exemplo:

```Kotlin
val data1 = LocalDate.of(2021, 10, 15)
val data2 = LocalDate.of(2021, 10, 20)
val resultado = data1.compareTo(data2)

println(resultado) // Saída: -5 (data1 é anterior a data2)
```

## Profundidade:

Comparar datas é uma tarefa antiga que se tornou ainda mais importante com o avanço da tecnologia e a necessidade de trabalhar com grandes conjuntos de dados. Existem diferentes formas de comparar datas, como o uso de operadores lógicos ou funções específicas da linguagem de programação utilizada.

Em Kotlin, a função `compareTo()` compara as datas com base em sua ordem cronológica. Se a data passada como argumento for anterior à data alvo, o resultado será um número negativo. Se for posterior, o resultado será um número positivo. Se as datas forem iguais, o resultado será 0.

## Veja também:

- Documentação oficial do Kotlin sobre `LocalDate`: https://kotlinlang.org/docs/datetime.html#comparing-two-localdate-instances
- Artigo sobre a importância de comparar datas na programação: https://www.geeksforgeeks.org/comparing-two-dates-in-java/