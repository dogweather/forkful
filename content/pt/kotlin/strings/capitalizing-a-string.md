---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:40.227377-07:00
description: "Como fazer: Em Kotlin, as strings podem ser capitalizadas usando as\
  \ fun\xE7\xF5es da biblioteca padr\xE3o, sem a necessidade de bibliotecas de terceiros.\
  \ A\u2026"
lastmod: '2024-03-13T22:44:46.524802-06:00'
model: gpt-4-0125-preview
summary: "Em Kotlin, as strings podem ser capitalizadas usando as fun\xE7\xF5es da\
  \ biblioteca padr\xE3o, sem a necessidade de bibliotecas de terceiros."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
Em Kotlin, as strings podem ser capitalizadas usando as funções da biblioteca padrão, sem a necessidade de bibliotecas de terceiros. A abordagem de Kotlin para manipulação de strings torna essas operações diretas e concisas.

### Capitalizando toda a string:
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Saída: HELLO, WORLD!
```

### Capitalizando apenas o primeiro caractere:
A partir do Kotlin 1.5, a função `capitalize()` foi depreciada e substituída por uma combinação de `replaceFirstChar` e uma lambda que verifica se é uma letra minúscula para transformá-la em maiúscula.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Saída: Hello, world!
```

Essa abordagem mantém o restante da frase em sua forma original, alterando apenas a primeira letra para maiúscula.
