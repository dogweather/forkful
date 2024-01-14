---
title:                "Kotlin: Obtendo a data atual"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é importante

Saber a data atual é uma tarefa comum em muitos aplicativos e sistemas de software. Isso pode ser útil para exibir informações atualizadas, agendar eventos e tarefas, registrar timestamps e muito mais.

## Como obter a data atual em Kotlin

Usando a linguagem de programação Kotlin, obter a data atual é uma tarefa fácil. Tudo o que precisamos fazer é importar a classe `java.util.Date` e criar uma instância dela.

```Kotlin
import java.util.Date

val dataAtual = Date()
println(dataAtual)
```

A saída deste código será algo parecido com `Sun Aug 08 13:12:22 BRT 2021`, dependendo do local e do fuso horário em que você estiver executando esse código.

Além disso, também é possível formatar a data de acordo com sua preferência usando a classe `java.text.SimpleDateFormat`.

```Kotlin
val dataAtual = Date()
val formato = SimpleDateFormat("dd/MM/yyyy")
println(formato.format(dataAtual))
```

Neste exemplo, a data atual será formatada no formato de dia, mês e ano separados por barras, resultando em algo como `08/08/2021`.

## Mergulhando mais fundo na obtenção da data atual

A classe `java.util.Date` é amplamente utilizada para obter a data atual, mas é importante mencionar que ela possui algumas limitações. Por exemplo, ela não fornece maneiras de trabalhar com horários e fusos horários diferentes.

Por esse motivo, a partir da versão 8 do Java, foi introduzida a classe `java.time.LocalDate`, que oferece métodos mais robustos para trabalhar com datas. Em Kotlin, podemos usá-la da seguinte maneira:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val dataAtual = LocalDate.now()
val formato = DateTimeFormatter.ofPattern("dd/MM/yyyy")
println(formato.format(dataAtual))
```

O resultado final será o mesmo que o exemplo anterior, mas agora estamos utilizando uma classe mais moderna e específica para trabalhar com datas.

## Veja também

- [Documentação oficial do Kotlin sobre a classe Date](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Tutorial sobre a classe Date em Kotlin](https://www.geeksforgeeks.org/kotlin-date-class/)