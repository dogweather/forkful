---
title:    "Kotlin: Calculando uma data no futuro ou passado"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que?

Há várias razões pelas quais alguém pode querer calcular uma data no futuro ou passado. Pode ser para planejar uma viagem, gerenciar prazos de projetos ou simplesmente por curiosidade.

## Como fazer

A linguagem de programação Kotlin oferece várias maneiras de calcular uma data no futuro ou passado. Uma maneira simples é utilizando a classe ```LocalDate``` e o método ```plusDays()```, como mostrado no exemplo abaixo:

```Kotlin
val dataAtual = LocalDate.now()
val dataFutura = dataAtual.plusDays(10)

println(dataFutura) // Output: 2021-07-15
```

No código acima, criamos uma variável ```dataAtual``` que armazena a data atual e, em seguida, utilizamos o método ```plusDays()``` para adicionar 10 dias à data atual e armazená-la na variável ```dataFutura```. Por fim, imprimimos a data futura no console.

Outra maneira é utilizando a classe ```Calendar``` e o método ```add()```, como mostrado no exemplo abaixo:

```Kotlin
val calendario = Calendar.getInstance()
calendario.add(Calendar.DATE, 10)

println(calendario.time) // Output: Sat Jul 10 10:48:37 GMT 2021
```

Nesse código, criamos uma instância da classe ```Calendar```, que nos permite manipular datas. Utilizamos o método ```add()``` para adicionar 10 dias à data atual e, em seguida, imprimimos a data resultante utilizando o método ```time```.

## Detalhando mais

Caso você precise realizar cálculos mais complexos, é possível utilizar a classe ```LocalDateTime```, que permite manipular data e hora juntas. Além disso, Kotlin também possui a biblioteca ```java.time``` que oferece diversas classes e métodos para trabalhar com datas e horas de maneira mais precisa e eficiente.

## Veja também

- Documentação oficial do Kotlin sobre datas e horas: https://kotlinlang.org/docs/datetime.html
- Tutorial sobre manipulação de datas em Kotlin: https://www.programiz.com/kotlin-programming/datetime