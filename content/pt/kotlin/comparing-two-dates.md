---
title:                "Kotlin: Comparando duas datas"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas?

Comparar duas datas é uma tarefa comum em muitos projetos de programação. Ao entender como funciona a comparação de datas, você poderá trabalhar com mais eficiência com valores de data em suas aplicações. Além disso, ao dominar esse conceito, você poderá evitar bugs e imprecisões ao lidar com datas em seu código.

## Como Fazer

### Kotlin: 

```Kotlin
fun main(){
    val data1 = LocalDate.of(2021, 10, 23)
    val data2 = LocalDate.of(2021, 10, 21)
    val resultado = data2.compareTo(data1)
    println(resultado)
}
```

### Saída: 

`-2`

Neste exemplo, criamos duas variáveis ​​do tipo `LocalDate`, que armazenam duas datas diferentes. Em seguida, usamos o método `compareTo()` para comparar as duas datas e armazenamos o resultado em uma nova variável `resultado`. Por fim, imprimimos o resultado, que no caso do exemplo é `-2`.

## Mergulho Profundo

Para comparar duas datas em Kotlin, é preciso entender como o método `compareTo()` funciona. Ele retorna um valor inteiro que indica se a primeira data é anterior, igual ou posterior à segunda data. Se a primeira data for anterior à segunda, o valor retornado será negativo. Se as duas datas forem iguais, o valor retornado será zero. E se a primeira data for posterior à segunda, o valor retornado será positivo.

Além disso, existem outras formas de comparar datas em Kotlin, como o método `isBefore()` e `isAfter()`, que retornam um valor booleano indicando se a primeira data é antes ou depois da segunda data.

## Veja Também

- [Documentação oficial do Kotlin sobre datas](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Guia de comparação de datas em Kotlin](https://www.baeldung.com/kotlin/compare-dates)