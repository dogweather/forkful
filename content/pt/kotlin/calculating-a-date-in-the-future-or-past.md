---
title:                "Kotlin: Calculando uma data no futuro ou passado"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Saber como calcular uma data no futuro ou no passado pode ser extremamente útil em diversas situações, como no desenvolvimento de aplicativos, jogos ou em tarefas diárias. Além disso, entender como fazer esse cálculo pode ajudar na compreensão de conceitos básicos de programação.

## Como fazer

Para calcular uma data no Kotlin, é necessário usar a classe `LocalDate` da biblioteca padrão do Java. Essa classe permite que você trabalhe com datas sem a necessidade de realizar cálculos complexos manualmente.

Primeiro, você precisa importar a classe `LocalDate` e criar uma instância dela, passando como parâmetros o ano, mês e dia desejados. Por exemplo:

```Kotlin
import java.time.LocalDate

val dataFutura = LocalDate.of(2022, 1, 1)
```

Para calcular uma data no passado, basta passar valores negativos para o parâmetro do ano. Por exemplo:

```Kotlin
val dataPassada = LocalDate.of(-200, 12, 1)
```

Você também pode obter a data atual usando o método `now()`:

```Kotlin
val dataAtual = LocalDate.now()
```

Uma vez que você tenha a data desejada, é possível realizar operações matemáticas com ela. Por exemplo, para adicionar 1 dia à data atual, você pode usar o método `plusDays()`:

```Kotlin
val dataFutura = dataAtual.plusDays(1)
```

Para subtrair 2 meses da data atual, o código seria:

```Kotlin
val dataPassada = dataAtual.minusMonths(2)
```

Como output, você teria:

```
data atual: 2021-10-18
data futura: 2021-10-19
data passada: 2021-08-18
```

## Aprofundamento

Existem diversas outras classes e métodos relacionados a datas disponíveis no Kotlin, como `LocalTime` para trabalhar com horas, `LocalDateTime` para datas e horas combinadas e `Period` para cálculos de períodos entre datas.

Além disso, é importante ter em mente que o Kotlin utiliza o calendário gregoriano como padrão, mas é possível trabalhar com outros calendários passando um `Chronology` como parâmetro na criação da data.

Veja mais informações sobre as classes e métodos relacionados a datas no site oficial do Kotlin.

## Veja também

- [Documentação oficial do Kotlin: Classes de datas e horas](https://kotlinlang.org/docs/dates.html)
- [Tutorial sobre trabalho com datas no Kotlin](https://www.section.io/engineering-education/kotlin-time-and-date/)
- [Exemplos práticos de uso de datas no Kotlin](https://blog.mindorks.com/dates-and-times-with-kotlin)