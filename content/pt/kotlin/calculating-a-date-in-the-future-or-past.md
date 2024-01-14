---
title:                "Kotlin: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular datas no futuro ou no passado?

Há muitas situações em que é necessário calcular uma data no futuro ou no passado. Por exemplo, em um aplicativo de calendário, é essencial calcular as datas de eventos futuros ou lembrá-lo de eventos passados importantes. Também pode ser necessário calcular o prazo de entrega de um projeto ou o vencimento de uma conta. Em resumo, calcular datas no futuro ou no passado é uma habilidade valiosa em programação e pode ser útil em diferentes situações.

## Como calcular datas no futuro ou no passado com Kotlin

Para calcular datas no futuro ou no passado com Kotlin, usaremos a classe `LocalDate` da biblioteca `java.time` do Java. Esta classe representa uma data (ano, mês e dia) sem informações de hora e fuso horário. Vamos supor que queremos calcular a data daqui a 2 semanas a partir de hoje. O código em Kotlin ficaria assim:

```Kotlin
import java.time.LocalDate

fun main() {
  val dataAtual = LocalDate.now() //pega a data atual
  val dataCalculada = dataAtual.plusWeeks(2) //adiciona 2 semanas à data atual
  println("A data daqui a 2 semanas será: $dataCalculada")
}
//-- output: A data daqui a 2 semanas será: 2021-07-14
```

Podemos passar outros valores como parâmetros para os métodos `plusYears()`, `plusMonths()` e `plusDays()` para calcular datas mais distantes ou próximas. Também podemos calcular datas no passado usando os métodos `minusYears()`, `minusMonths()` e `minusDays()`.

## Profundidade no calculo de datas no futuro ou no passado

Além dos métodos utilizados no exemplo acima, a classe `LocalDate` possui uma variedade de outros métodos úteis para realizar cálculos com datas. Alguns exemplos são `withYear()`, `withMonth()` e `withDayOfMonth()`, que permitem alterar o ano, mês ou dia de uma data. Também existem métodos para comparar datas, como `isAfter()` e `isBefore()`, e para verificar se um ano é bissexto, como `isLeapYear()`. É importante mencionar que a classe `LocalDate` é imutável, ou seja, após criar uma data, ela não pode ser alterada. Por isso, todos os métodos retornam uma nova instância da classe `LocalDate` com as alterações desejadas.

## Veja também

- Documentação oficial do Java sobre a classe `LocalDate` - https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Outras formas de trabalhar com datas no Kotlin - https://kotlinlang.org/docs/datetime.html
- Exemplo de aplicação prática do cálculo de datas no futuro em um aplicativo Android - https://medium.com/swlh/calendaring-with-kotlin-kiloloco-github-io-70a5f4690292