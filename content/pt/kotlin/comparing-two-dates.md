---
title:    "Kotlin: Comparando duas datas"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar duas datas é uma tarefa comum em programação, pois permite que os desenvolvedores determinem a diferença entre dois momentos específicos no tempo. Isso pode ser útil em muitos casos, como em aplicações de reserva de passagens ou em sistemas de gerenciamento de projetos.

## Como fazer?

Codificar a comparação de duas datas em Kotlin é simples e pode ser feito utilizando os métodos da classe `LocalDate` e `ChronoUnit`. Primeiro, vamos criar duas instâncias da classe `LocalDate`, representando as datas que queremos comparar:

```Kotlin
val data1 = LocalDate.of(2021, 10, 1)
val data2 = LocalDate.of(2021, 10, 10)
```

Agora, podemos utilizar o método `between` da classe `ChronoUnit` para determinar a diferença entre as datas em uma unidade específica, como dias, semanas ou meses:

```Kotlin
val diferencaDias = ChronoUnit.DAYS.between(data1, data2)
val diferencaSemanas = ChronoUnit.WEEKS.between(data1, data2)
val diferencaMeses = ChronoUnit.MONTHS.between(data1, data2)
```

O resultado da comparação será então armazenado em uma variável do tipo `Long`. O código completo seria algo como isso:

```Kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main(args: Array<String>) {
    val data1 = LocalDate.of(2021, 10, 1)
    val data2 = LocalDate.of(2021, 10, 10)

    val diferencaDias = ChronoUnit.DAYS.between(data1, data2)
    val diferencaSemanas = ChronoUnit.WEEKS.between(data1, data2)
    val diferencaMeses = ChronoUnit.MONTHS.between(data1, data2)

    println("Diferença em dias: \${diferencaDias}")
    println("Diferença em semanas: \${diferencaSemanas}")
    println("Diferença em meses: \${diferencaMeses}")
}
```

A saída do código acima seria:

```
Diferença em dias: 9
Diferença em semanas: 1
Diferença em meses: 0
```

## Mais sobre a comparação de datas

Além de determinar a diferença entre duas datas, também é possível comparar se uma data é anterior ou posterior a outra. Isso pode ser feito utilizando o método `isAfter` ou `isBefore` da classe `LocalDate`:

```Kotlin
val data1 = LocalDate.of(2021, 10, 1)
val data2 = LocalDate.of(2021, 10, 10)

val data1AntesData2 = data1.isBefore(data2)
val data2AntesData1 = data2.isBefore(data1)

println("Data1 é anterior a Data2: \${data1AntesData2}")
println("Data2 é anterior a Data1: \${data2AntesData1}")
```

A saída seria:

```
Data1 é anterior a Data2: true
Data2 é anterior a Data1: false
```

Também é possível comparar a igualdade entre duas datas utilizando o método `isEqual`:

```Kotlin
val data1 = LocalDate.of(2021, 10, 1)
val data2 = LocalDate.of(2021, 10, 10)

val datasIguais = data1.isEqual(data2)
```

A saída seria:

```
Datas são iguais: false
```

## Veja também

- [Documentação do Kotlin sobre a classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/)
- [Tutorial sobre como trabalhar com datas em Kotlin](https://www.baeldung.com/kotlin/dates)
- [Vídeo no YouTube sobre comparação de datas em Kotlin](https://www.youtube.com/watch?v=iBE43XZy_eQ)