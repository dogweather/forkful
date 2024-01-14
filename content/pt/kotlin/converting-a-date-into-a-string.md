---
title:    "Kotlin: Convertendo uma data em uma string"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string pode ser útil em diversas situações, como quando é necessário exibir a data em um determinado formato ou quando é necessário realizar operações com datas no código.

## Como fazer:

Para converter uma data em uma string em Kotlin, podemos utilizar a função `format()` do objeto `SimpleDateFormat`. Veja o exemplo abaixo:

```
val data = Date()
val formato = SimpleDateFormat("dd/MM/yyyy")
val dataString = formato.format(data)

println(dataString) // output: 13/08/2021
```

Nesse exemplo, criamos um objeto `SimpleDateFormat` com o formato "dd/MM/yyyy" e utilizamos a função `format()` para converter a data atual em uma string nesse formato. É importante lembrar que o formato utilizado deve seguir o padrão da classe `SimpleDateFormat`.

## Mergulhando mais fundo:

Ao utilizar a função `format()`, podemos especificar diferentes padrões de formatação para a string resultante, como por exemplo incluir o nome do mês ou do dia da semana na string. Além disso, também podemos utilizar o objeto `Calendar` para obter informações mais detalhadas sobre a data, como o dia da semana, o número da semana no ano, etc.

## Veja também:
- [Documentação do Kotlin sobre a classe SimpleDateFormat](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/)
- [Tutorial sobre formatação de datas e horas em Kotlin](https://www.baeldung.com/kotlin/dates)