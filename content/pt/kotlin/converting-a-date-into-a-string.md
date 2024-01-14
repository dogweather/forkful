---
title:                "Kotlin: Convertendo uma data em uma string"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Programar em Kotlin frequentemente envolve trabalhar com datas e horários de diferentes formatos. Para tornar esses dados mais legíveis e utilizáveis, uma técnica comum é converter uma data em uma string. Isso permite que o programador personalize o formato da data de acordo com as necessidades do sistema ou do usuário final.

## Como fazer

Para converter uma data em uma string em Kotlin, podemos usar o método `format` da classe `SimpleDateFormat`. Esse método aceita dois parâmetros: o primeiro é o formato de data desejado e o segundo é a data que será convertida. Veja o exemplo abaixo:

```Kotlin
val currentDate = Date()
val dateFormat = SimpleDateFormat("dd/MM/yyyy")

val convertedDate = dateFormat.format(currentDate)

println("Data atual em formato de string: $convertedDate")
```

Esse código irá imprimir a data atual em formato de string no padrão "dd/MM/aaaa". É importante lembrar que esse padrão pode variar de acordo com a localização do sistema.

## Profundidade

A classe `SimpleDateFormat` permite uma série de opções para personalizar o formato da data. Além de especificar dia, mês e ano, é possível adicionar informações como hora, minutos e segundos. Outra opção interessante é a adição de abreviações de dias da semana ou meses. O código abaixo mostra algumas possibilidades:

```Kotlin
val currentDate = Date()
val dateFormat = SimpleDateFormat("dd 'de' MMM 'de' yyyy 'às' HH:mm:ss")

val convertedDate = dateFormat.format(currentDate)

println("Data atual em formato de string: $convertedDate")
```

Esse código irá imprimir a data e hora atuais em formato de string no padrão "dd de MMM de yyyy às HH:mm:ss", que ficaria algo como "27 de mar. de 2021 às 14:23:45".

## Veja também

- [Documentação oficial do Kotlin sobre datas e horários](https://kotlinlang.org/docs/datetime.html)
- [Tutorial do DevMedia sobre formatação de datas em Kotlin](https://www.devmedia.com.br/java-date-conhecendo-a-classe-simpledateformat/29034)
- [Exemplos de formatação de datas em Kotlin no Stack Overflow](https://stackoverflow.com/questions/51607003/how-to-format-a-date-string-in-kotlin)