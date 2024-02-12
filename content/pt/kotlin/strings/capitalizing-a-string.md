---
title:                "Capitalizando uma string"
aliases: - /pt/kotlin/capitalizing-a-string.md
date:                  2024-02-03T19:05:40.227377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?

Capitalizar uma string na programação envolve converter o primeiro caractere da string em maiúsculo, caso ele não seja, o que é útil para formatar entradas de usuário ou exibir texto em uma interface de usuário de maneira mais padronizada ou amigável. Os programadores realizam essa operação para garantir a consistência dos dados ou para atender a requisitos específicos de formatação dentro de suas aplicações de software.

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
