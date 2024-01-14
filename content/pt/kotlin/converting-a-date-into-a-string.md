---
title:    "Kotlin: Convertendo uma data em uma string"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string é uma tarefa comum na programação e pode ser útil por diversos motivos, como armazenar uma data em um banco de dados ou exibir informações de datas em um formato legível para o usuário.

## Como fazer:

```Kotlin
// Convertendo uma data atual em uma string:
val dataAtual = LocalDate.now().format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
// Saída: 09/10/2021

// Convertendo uma data específica em uma string:
val dataEspecifica = LocalDate.of(2021, 10, 9).format(DateTimeFormatter.ofPattern("dd MMMM yyyy"))
// Saída: 09 outubro 2021
```

## Mergulho Profundo:

Em Kotlin, a conversão de uma data em uma string é feita utilizando a classe `LocalDate` e o método `format()`. É possível passar diferentes padrões para o método `ofPattern()`, que determinará como a data será formatada. Alguns padrões comuns incluem "dd/MM/yyyy" para exibir a data no formato de dia/mês/ano e "dd MMMM yyyy" para exibir o mês por extenso.

Além disso, é importante lembrar de importar a classe `LocalDate` e a classe `DateTimeFormatter` antes de utilizar esse método.

## Veja também:

- Documentação oficial do Kotlin sobre a classe `LocalDate`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time/
- Documentação oficial do Kotlin sobre a classe `DateTimeFormatter`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/