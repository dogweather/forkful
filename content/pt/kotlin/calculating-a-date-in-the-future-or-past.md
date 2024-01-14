---
title:    "Kotlin: Calculando uma data no futuro ou passado"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou passado pode ser útil em diversas situações, como por exemplo, em um aplicativo de agenda ou em um sistema de reservas. Além disso, entender como fazer esse cálculo pode melhorar suas habilidades de programação em Kotlin.

## Como Fazer

Para calcular uma data no futuro ou passado em Kotlin, utilizamos a classe `Calendar` e o método `add()`. Veja o código abaixo para um exemplo de como calcular um dia a partir de hoje:

```Kotlin
// Obtém a data atual
val today = Calendar.getInstance()

// Adiciona 1 dia à data atual
today.add(Calendar.DAY_OF_MONTH, 1)

// Exibe a data resultante
println(today.time)
```

O output desse código será a data de amanhã, no formato `Mon May 10 14:20:36 BRT 2021`.

## Deep Dive

A classe `Calendar` é uma das formas de trabalhar com data e hora em Kotlin. Ela permite manipular diversos campos, como ano, mês, dia, hora, minuto e segundo. Além disso, o método `add()` pode receber como argumento qualquer um desses campos, permitindo um cálculo preciso e flexível da data. Veja mais alguns exemplos abaixo:

```Kotlin
// Calcular 1 hora no futuro
today.add(Calendar.HOUR_OF_DAY, 1)

// Calcular 2 semanas atrás
today.add(Calendar.WEEK_OF_MONTH, -2)

// Calcular 1 ano no futuro
today.add(Calendar.YEAR, 1)
```

Além da classe `Calendar`, Kotlin também possui outras formas de trabalhar com data e hora, como a classe `LocalDateTime` e a API `java.time`. É importante entender as diferentes opções e escolher a melhor para cada situação.

## Veja Também

- [Documentação oficial do método `add()` da classe `Calendar` em inglês](https://developer.android.com/reference/java/util/Calendar.html#add(int,%20int))
- [Tutorial completo sobre data e hora em Kotlin em português](https://blog.kotlinacademy.com/pt/tutorial-kotlin-data-e-hora-4d4109a522a5)

Com essas informações, você já sabe como calcular datas no futuro ou passado em Kotlin. Lembre-se de sempre ler a documentação oficial e utilizar as diferentes opções da linguagem para resolver seus problemas de programação. Mantenha-se atualizado e continue aprimorando suas habilidades em Kotlin.