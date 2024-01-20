---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

"Parsing" uma data de uma string é o processo de extrair informações de data e hora de texto formatado. Isso é feito por programadores quando eles precisam interpretar datas fornecidas em texto, como entrada de usuário ou dados de arquivos.

## Como fazer:

No Kotlin, nós fazemos parse de uma data de uma string usando `LocalDate.parse()`. Aqui está um exemplo simples:

```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dataString = "2022-10-21"
    val formato = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val data = LocalDate.parse(dataString, formato)
    
    println(data)
}
```

A saída será: '2022-10-21'.

## Detalhamento

1. **Contexto Histórico**: Antes do Java 8, o processo de parsing de datas era muito complexo e dependente do `SimpleDateFormat`, que tem muitos problemas conhecidos. Kotlin adotou as APIs `java.time` do Java 8, que são muito mais fáceis e seguras.

2. **Alternativas**: Por vezes, você pode encontrar datas em formatos não padronizados. Nestes casos é necessário criar um `DateTimeFormatter` personalizado, como mostrado em nosso exemplo.

3. **Detalhes de Implementação**: O método `LocalDate.parse` usa um `DateTimeFormatter` para interpretar as datas. Você pode criar seu próprio formato ou usar um dos muitos formatos predefinidos.

## Veja Também

- Documentação oficial do Kotlin sobre a Classe LocalDate: [Tutorial Kotlin LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)[PT]

- Guia rápido para Parsing, formatação e manipulação de datas no Java 8: [Guia Java 8 Date](https://www.baeldung.com/java-8-date-time-intro) [EN]
