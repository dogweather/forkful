---
title:    "Kotlin: Comparando duas datas"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Kotlin?

Comparar duas datas é uma tarefa comum em muitos aplicativos e programas. Por exemplo, pode ser necessário verificar se uma data de nascimento é maior que a data atual, ou se o prazo de um projeto já passou. Comparar datas permite que você crie lógicas e condições com base em valores de data, o que pode ser muito útil no desenvolvimento de aplicativos.

## Como comparar duas datas em Kotlin

Para comparar duas datas em Kotlin, você pode usar a classe `LocalDate`, que representa uma data sem hora e fuso horário. Vamos ver um exemplo de código:

```Kotlin
import java.time.LocalDate

fun main() {
    // criando duas datas
    val data1 = LocalDate.of(2019, 5, 12)
    val data2 = LocalDate.now()

    // comparando as datas
    if (data1.isAfter(data2)) {
        println("Data 1 é depois da data 2")
    } else {
        println("Data 1 é antes ou igual à data 2")
    }
}
```

Neste exemplo, nós criamos duas datas utilizando o método `of` da classe `LocalDate`, que recebe como parâmetros o ano, mês e dia. Em seguida, utilizamos o método `isAfter` para comparar se a data 1 é depois da data 2. Você também pode usar outros métodos, como `isBefore` e `isEqual`, para fazer diferentes tipos de comparações entre datas.

O resultado desse código seria a saída "Data 1 é antes ou igual à data 2", pois a data 1 é anterior à data atual.

## Uma análise mais detalhada sobre a comparação de datas

Além dos métodos mencionados anteriormente, a classe `LocalDate` possui diversos outros métodos que podem ser úteis na comparação de datas, como `minusDays`, `plusWeeks`, entre outros. Além disso, você também pode utilizar a classe `LocalTime` para trabalhar com horas em suas comparações.

É importante lembrar que, para comparar datas e obter resultados precisos, é necessário levar em consideração o fuso horário e o calendário utilizado em cada data. Portanto, é importante estar atento a esses detalhes ao trabalhar com datas em seu código.

## Veja também

- [Documentação do Kotlin sobre datas e horas](https://kotlinlang.org/docs/datetime.html)
- [Tutorial de comparação de datas em Kotlin](https://www.baeldung.com/kotlin/comparing-dates)
- [Lista de bibliotecas úteis para trabalhar com datas em Kotlin](https://www.baeldung.com/kotlin-date-time-libraries)