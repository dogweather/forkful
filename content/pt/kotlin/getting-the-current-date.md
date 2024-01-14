---
title:                "Kotlin: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é importante

Obter a data atual é uma tarefa comum em muitos projetos de programação. Pode ser útil em situações como registrar a data de criação de um arquivo ou armazenar a data de um evento.

## Como fazer

Existem várias maneiras de obter a data atual em Kotlin. A maneira mais simples é usar a classe `LocalDate` do pacote `java.time` e o método `now()` para obter a data atual.

```Kotlin
import java.time.LocalDate

val dataAtual = LocalDate.now()
println(dataAtual) // 2021-05-25
```

Podemos personalizar o formato de data usando o método `format()` com um objeto `DateTimeFormatter`.

```Kotlin
import java.time.format.DateTimeFormatter  

val dataAtual = LocalDate.now()
val dataFormatada = dataAtual.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println(dataFormatada) // 25/05/2021
```

Se quisermos obter a data e a hora atuais, podemos usar a classe `LocalDateTime`.

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

val dateTimeAtual = LocalDateTime.now()
val dateTimeFormatado = dateTimeAtual.format(DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))
println(dateTimeFormatado) // 25/05/2021 15:30:45
```

## Mergulho profundo

A classe `LocalDate` possui muitos métodos úteis para trabalhar com datas, como `plusDays()`, `minusMonths()`, `isBefore()` e `isAfter()`. Também podemos utilizar outras classes como `LocalTime` e `ZoneDateTime` para obter mais opções de manipulação de data e hora.

Outra maneira de obter a data atual é usando a classe `Calendar` do Java.

```Kotlin
import java.util.Calendar

val dataAtual = Calendar.getInstance().time
println(dataAtual) // Tue May 25 15:30:45 GMT 2021
```

Por fim, também podemos usar a biblioteca `Joda Time`, que possui uma sintaxe intuitiva e muitas opções para trabalhar com data e hora.

## Veja também

- Guia de referência para a classe `LocalDate`: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- Documentação da classe `Calendar`: [https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- Utilizando a biblioteca `Joda Time` em Kotlin: [https://www.baeldung.com/kotlin/jodatime](https://www.baeldung.com/kotlin/jodatime)