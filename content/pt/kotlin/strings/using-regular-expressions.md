---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:32.989217-07:00
description: 'Como Fazer: #.'
lastmod: '2024-03-13T22:44:46.531453-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Usando express\xF5es regulares"
weight: 11
---

## Como Fazer:


### Correspondência Básica
Para verificar se uma string corresponde a um padrão específico em Kotlin, você pode usar o método `matches` da classe `Regex`.

```kotlin
val pattern = "kotlin".toRegex()
val input = "Eu amo kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Saída: true
```

### Encontrando e Extraindo Partes da String
Se você deseja encontrar partes de uma string que correspondam a um padrão, em Kotlin, você pode iterar sobre todas as correspondências:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "A data de hoje é 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Saída: 07/09/2023
```

### Substituindo Texto
Substituir partes de uma string que correspondem a um padrão é simples com a função `replace`:

```kotlin
val input = "Nome do usuário: usuario123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Saída: Nome do usuário: usuarioXXX
```

### Dividindo Strings
Divida uma string em uma lista, usando um padrão regex como delimitador:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Saída: [1, 2, 3, 4, 5]
```

### Bibliotecas de Terceiros: Kotest
[Kotest](https://github.com/kotest/kotest) é uma biblioteca de testes Kotlin popular que estende o suporte regex integrado de Kotlin, particularmente útil para validação em casos de teste.

```kotlin
// Assumindo que o Kotest foi adicionado ao seu projeto
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@teste.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Isso passará no teste se a entrada corresponder ao padrão de email.
```

Ao incorporar expressões regulares em suas aplicações Kotlin, você pode realizar processamento de texto sofisticado de forma eficiente. Seja validando entrada de usuário, extraindo dados ou transformando strings, padrões regex oferecem uma solução robusta.
