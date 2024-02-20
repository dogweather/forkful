---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:40.227377-07:00
description: "Capitalizar uma string na programa\xE7\xE3o envolve converter o primeiro\
  \ caractere da string em mai\xFAsculo, caso ele n\xE3o seja, o que \xE9 \xFAtil\
  \ para formatar\u2026"
lastmod: 2024-02-19 22:05:05.564680
model: gpt-4-0125-preview
summary: "Capitalizar uma string na programa\xE7\xE3o envolve converter o primeiro\
  \ caractere da string em mai\xFAsculo, caso ele n\xE3o seja, o que \xE9 \xFAtil\
  \ para formatar\u2026"
title: Capitalizando uma string
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
