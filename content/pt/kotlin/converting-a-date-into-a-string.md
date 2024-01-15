---
title:                "Convertendo uma data em uma string"
html_title:           "Kotlin: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Porquê

Você pode precisar converter uma data em uma string para exibi-la em um formato específico ou para salvá-la em um banco de dados. Com Kotlin, esse processo é simples e eficiente.

## Como Fazer

Usando a classe `SimpleDateFormat `, podemos converter uma data em uma string no formato desejado. Veja um exemplo a seguir:

```Kotlin
val data = Date() // Criando uma data atual
val formato = SimpleDateFormat("dd/MM/yyyy") // Definindo o formato desejado
val dataString = formato.format(data) // Convertendo a data em uma string usando o formato
println(dataString) // Imprimindo a data em formato de string
// Saída: 11/05/2021
```

Você também pode alterar o formato da string para incluir informações como hora, minutos ou até mesmo fuso horário. Veja um exemplo:

```Kotlin
val data = Date()
val formato = SimpleDateFormat("dd/MM/yyyy HH:mm:ss z") // Adicionando hora, minutos e fuso horário
val dataString = formato.format(data)
println(dataString)
// Saída: 11/05/2021 12:30:45 CDT
```

## Deep Dive

A classe `SimpleDateFormat` possui vários padrões de formatação de data, que podem ser encontrados na documentação oficial do Kotlin. Além disso, também é possível definir um padrão personalizado, caso os pré-definidos não atendam às suas necessidades. Veja como criar um padrão personalizado:

```Kotlin
val data = Date()
val padrao = "dd 'de' MMMM 'de' yyyy 'às' HH:mm:ss" // Definindo um padrão personalizado
val formato = SimpleDateFormat(padrao)
val dataString = formato.format(data)
println(dataString)
// Saída: 11 de maio de 2021 às 12:30:45
```

Outro aspecto importante a ser lembrado é que a classe `SimpleDateFormat` não é thread-safe, o que significa que ela não deve ser usada em threads concorrentes. Se isso for necessário, é recomendado o uso da classe `DateTimeFormatter`, que é thread-safe.

## Veja Também

- Documentação oficial do Kotlin sobre a classe `SimpleDateFormat`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/
- Tutorial sobre formatação de datas com Kotlin: https://www.baeldung.com/kotlin-date-time-format
- Outras opções de classes para lidar com datas em Kotlin: https://developer.android.com/reference/java/time/package-summary