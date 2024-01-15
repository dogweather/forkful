---
title:                "Obtendo a data atual"
html_title:           "Kotlin: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Obter a data atual é uma tarefa comum em muitos aplicativos, desde um simples cronômetro até sistemas de reservas e agendamentos. Além disso, ter a capacidade de obter a data atual é essencial para garantir que as informações mostradas aos usuários sejam precisas e atualizadas.

## Como Fazer

Para obter a data atual em Kotlin, podemos usar a classe `LocalDate` da biblioteca padrão do Kotlin. Primeiro, vamos importar a classe no topo do nosso arquivo:

```Kotlin
import java.time.LocalDate
```

Em seguida, podemos simplesmente criar uma instância da classe `LocalDate` e usá-la para obter a data atual:

```Kotlin
val currentDate = LocalDate.now()
```

Podemos formatar a data da forma desejada usando o método `format()`:

```Kotlin
val formattedDate = currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
```

Agora, podemos usar a variável `formattedDate` para exibir a data atual em nosso aplicativo.

## Mergulho Profundo

A classe `LocalDate` faz parte do pacote `java.time` introduzido no Java 8 e agora disponível também para o Kotlin. Essa classe representa uma data no sistema de calendário padrão ISO (International Organization for Standardization). Além de obter a data atual, a classe `LocalDate` também possui vários métodos úteis, como `plusDays()`, `plusMonths()` e `plusYears()`, que permitem adicionar dias, meses e anos à data atual.

Também podemos criar uma instância da classe `LocalDate` especificando o ano, mês e dia desejados:

```Kotlin
val specificDate = LocalDate.of(2020, 8, 1)
```

Além disso, a classe `LocalDate` também lida com questões de fuso horário e horário de verão automaticamente, o que a torna uma escolha confiável para obter a data atual em diferentes regiões do mundo.

## Veja Também

- Documentação oficial do pacote `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Introdução ao pacote `java.time` no Kotlin: https://kotlinlang.org/docs/tutorials/datetime.html
- Tutorial sobre como obter a data atual em Kotlin: https://www.baeldung.com/kotlin/get-current-date-time