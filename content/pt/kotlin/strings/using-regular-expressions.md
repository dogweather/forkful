---
title:                "Usando expressões regulares"
aliases:
- /pt/kotlin/using-regular-expressions.md
date:                  2024-02-03T19:17:32.989217-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Que?

Expressões regulares (regex) são uma ferramenta poderosa para o processamento de texto, permitindo que programadores busquem, correspondam e manipulem strings com técnicas avançadas de correspondência de padrões. Em Kotlin, utilizar regex ajuda a executar de forma eficiente tarefas complexas de processamento de texto como validação, análise sintática ou transformação, tornando-se indispensável para tarefas que vão desde a simples manipulação de strings até a análise de texto complexa.

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
