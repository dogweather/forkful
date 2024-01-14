---
title:                "Kotlin: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por que fazer a conversão de uma data para uma string?

A conversão de uma data para uma string é uma tarefa comum em programação. Ao converter uma data em um formato legível para humanos, podemos tornar as informações mais compreensíveis e apresentá-las de forma mais amigável ao usuário. Além disso, a conversão para string é útil quando precisamos armazenar ou transmitir a data em um banco de dados ou em um formato de arquivo.

## Como fazer a conversão em Kotlin

Existem algumas maneiras de fazer a conversão de uma data em uma string em Kotlin. A seguir, mostraremos dois exemplos usando diferentes métodos e a saída esperada.

```
Kotlin // Exemplo com o uso do método toString()

val data = Date() //obtém a data atual
val dataString = data.toString() //converte a data em uma string
println(dataString)

// Saída: Sat Apr 17 17:29:55 GMT 2021
```

```
Kotlin // Exemplo com o uso da classe SimpleDateFormat

import java.text.SimpleDateFormat
import java.util.*

val data = Date() //obtém a data atual
val formato = SimpleDateFormat("dd/MM/yyyy") //especifica o formato desejado
val dataString = formato.format(data) //converte a data em uma string
println(dataString)

// Saída: 17/04/2021
```

## Aprofundando na conversão de data para string em Kotlin

Ao realizar a conversão, é importante estar ciente do formato da data que está sendo usada, pois isso influenciará no resultado final. Alguns formatos de data comumente usados são "dd/MM/yyyy", "MM/dd/yyyy", "dd/MM/yyyy HH:mm:ss" e "EEE, d MMM yyyy" (formato abreviado para dias da semana).

Além disso, é possível personalizar a saída, como adicionar informações de horário ou até mesmo traduzir o nome do dia da semana para o idioma desejado. Para isso, é necessário utilizar a classe SimpleDateFormat e seus métodos de formatação.

## Veja também

- [Documentação oficial do Kotlin sobre formatação de datas e horas](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date-format/)
- [Tutorial sobre data e hora em Kotlin](https://devexperto.com/data-y-time-kotlin/)
- [Exemplos de formatação de datas em diferentes idiomas com Kotlin](https://www.baeldung.com/java-string-format-dates)