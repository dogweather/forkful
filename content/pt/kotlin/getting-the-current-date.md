---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pegando a Data Atual no Kotlin: O que e Por quê?

## O que e Por quê?
Ter a capacidade de extrair a data atual em Kotlin permite que você marque eventos, grave timestamps e realize cálculos com datas. É uma tarefa comum que os programadores fazem para acompanhar e organizar os dados com base no tempo.

## Como fazer:
Vamos para os exemplos de código. Em Kotlin, você pode obter a data atual usando a classe LocalDate em java.time. 

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val current = LocalDateTime.now()

    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")
    val formatted = current.format(formatter)

    println("Data Atual: $formatted")
}
```
Quando você rodar este código, você irá ter uma saída semelhante a:
```
Data Atual: 19-09-2021 17:56:27
```

## Mergulhando Fundo
O pacote java.time foi adicionado no Java 8 para lidar com data e hora. Antes disso, os desenvolvedores usavam java.util.Date ou java.util.Calendar, que podem ser complexos e difíceis de manusear.

Em relação as alternativas, em Kotlin você também pode usar a classe java.util.Date para obter a data atual:

```Kotlin
import java.util.Date
import java.text.SimpleDateFormat

fun main() {
    val date = Date()
    val formatter = SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
    val formatted = formatter.format(date)

    println("Data Atual: $formatted")
}
```
Este código produzirá a mesma saída que o exemplo anterior.

Agora sobre a implementação. Quando você chama `LocalDateTime.now()`, o Kotlin pega a data e hora do relógio do sistema. O formato padrão é "yyyy-MM-dd-HH-mm-ss.zzz". Então, usamos `DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")` para formatar a data e hora no formato desejado.

## Veja Também
- [Documentação Oficial Kotlin - Pacote java.time](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/)
- [Tutorial de Java - Date-Time API ](https://docs.oracle.com/javase/tutorial/datetime/)
- [Blog da Baeldung - Como obter a data / hora atual em Java] (https://www.baeldung.com/java-8-date-time-intro)